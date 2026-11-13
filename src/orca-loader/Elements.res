open Types
open ErrorUtils
open Identity
open Utils
open EventListenerManager
open ApplePayTypes


let make = (
  options,
  setIframeRef,
  ~clientSecret,
  ~sdkSessionId,
  ~publishableKey,
  ~logger: option<OrcaLogger.loggerMake>,
  ~analyticsMetadata,
  ~customBackendUrl,
) => {
  let applePaySessionRef = ref(Nullable.null)
  try {
    let iframeRef = []
    let logger = logger->Option.getOr(OrcaLogger.defaultLoggerConfig)
    let savedPaymentElement = Dict.make()
    let localOptions = options->JSON.Decode.object->Option.getOr(Dict.make())

    let endpoint = ApiEndpoint.getApiEndPoint(~publishableKey, ())
    let redirect = ref("if_required")

    let appearance =
      localOptions->Dict.get("appearance")->Option.getOr(Dict.make()->JSON.Encode.object)
    let launchTime = localOptions->getFloat("launchTime", 0.0)

    let fonts =
      localOptions
      ->Dict.get("fonts")
      ->Option.flatMap(JSON.Decode.array)
      ->Option.getOr([])
      ->JSON.Encode.array

    let blockConfirm =
      GlobalVars.isInteg &&
      options
      ->JSON.Decode.object
      ->Option.flatMap(x => x->Dict.get("blockConfirm"))
      ->Option.flatMap(JSON.Decode.bool)
      ->Option.getOr(false)
    let switchToCustomPod =
      GlobalVars.isInteg &&
      options
      ->JSON.Decode.object
      ->Option.flatMap(x => x->Dict.get("switchToCustomPod"))
      ->Option.flatMap(JSON.Decode.bool)
      ->Option.getOr(false)

    let localSelectorString = "hyper-preMountLoader-iframe"
    let mountPreMountLoaderIframe = () => {
      // obsidian: a pre-mount loader still on the page belongs to an earlier elements() call and
      // fetched that call's payment (its client secret). Reusing it left this payment's requests
      // unanswered once it had stopped listening, and the element stayed on its loading
      // placeholders. Replace it with one for this payment.
      [`#orca-element-${localSelectorString}`, `#orca-payment-element-iframeRef-${localSelectorString}`]
      ->Array.forEach(selector =>
        switch Window.querySelector(selector)->Nullable.toOption {
        | Some(stale) => stale->remove
        | None => ()
        }
      )
      if (
        Window.querySelector(
          `#orca-payment-element-iframeRef-${localSelectorString}`,
        )->Js.Nullable.isNullable
      ) {
        let componentType = "preMountLoader"
        let iframeDivHtml = `<div id="orca-element-${localSelectorString}" style= "height: 0px; width: 0px; display: none;"  class="${componentType}">
          <div id="orca-fullscreen-iframeRef-${localSelectorString}"></div>
           <iframe
           id ="orca-payment-element-iframeRef-${localSelectorString}"
           name="orca-payment-element-iframeRef-${localSelectorString}"
          src="${ApiEndpoint.sdkDomainUrl}/index.html?fullscreenType=${componentType}&publishableKey=${publishableKey}&clientSecret=${clientSecret}&sessionId=${sdkSessionId}&endpoint=${endpoint}"
          allow="*"
          name="orca-payment"
        ></iframe>
        </div>`
        let iframeDiv = Window.createElement("div")
        iframeDiv->Window.innerHTML(iframeDivHtml)
        Window.body->Window.appendChild(iframeDiv)
      }

      let elem = Window.querySelector(`#orca-payment-element-iframeRef-${localSelectorString}`)
      elem
    }

    let locale = localOptions->getJsonStringFromDict("locale", "")
    let loader = localOptions->getJsonStringFromDict("loader", "")
    let clientSecret = localOptions->getRequiredString("clientSecret", "", ~logger)
    let clientSecretReMatch = Re.test(`.+_secret_[A-Za-z0-9]+`->Re.fromString, clientSecret)

    let preMountLoaderIframeDiv = mountPreMountLoaderIframe()

    let unMountPreMountLoaderIframe = () => {
      switch preMountLoaderIframeDiv->Nullable.toOption {
      | Some(iframe) => iframe->remove
      | None => ()
      }
    }

    let preMountLoaderMountedPromise = Promise.make((resolve, _reject) => {
      let preMountLoaderIframeCallback = (ev: Types.event) => {
        let json = ev.data->Identity.anyTypeToJson
        let dict = json->getDictFromJson
        if dict->Dict.get("preMountLoaderIframeMountedCallback")->Option.isSome {
          resolve(true->JSON.Encode.bool)
        } else if dict->Dict.get("preMountLoaderIframeUnMount")->Option.isSome {
          unMountPreMountLoaderIframe()
        }
      }
      addSmartEventListener(
        "message",
        preMountLoaderIframeCallback,
        "onPreMountLoaderIframeCallback",
      )
    })

    let fetchPaymentsList = (mountedIframeRef, componentType) => {
      let handlePaymentMethodsLoaded = (event: Types.event) => {
        let json = event.data->Identity.anyTypeToJson
        let dict = json->getDictFromJson
        let isPaymentMethodsData = dict->getString("data", "") === "payment_methods"
        if isPaymentMethodsData {
          let json = dict->getJsonFromDict("response", JSON.Encode.null)
          // obsidian: upstream loaded TrustPay's script here for Apple Pay and Google Pay through the
          // TrustPay connector; the connector was removed.
          let msg = [("paymentMethodList", json)]->Dict.fromArray
          mountedIframeRef->Window.iframePostMessage(msg)
        }
      }
      let msg = [("sendPaymentMethodsResponse", true->JSON.Encode.bool)]->Dict.fromArray
      addSmartEventListener(
        "message",
        handlePaymentMethodsLoaded,
        `onPaymentMethodsLoaded-${componentType}`,
      )
      preMountLoaderIframeDiv->Window.iframePostMessage(msg)
    }

    let fetchCustomerPaymentMethods = (
      mountedIframeRef,
      disableSavedPaymentMethods,
      componentType,
    ) => {
      if !disableSavedPaymentMethods {
        let handleCustomerPaymentMethodsLoaded = (event: Types.event) => {
          let json = event.data->Identity.anyTypeToJson
          let dict = json->getDictFromJson
          let isCustomerPaymentMethodsData =
            dict->getString("data", "") === "customer_payment_methods"
          if isCustomerPaymentMethodsData {
            let json = dict->getJsonFromDict("response", JSON.Encode.null)
            let msg = [("customerPaymentMethods", json)]->Dict.fromArray
            mountedIframeRef->Window.iframePostMessage(msg)
          }
        }
        addSmartEventListener(
          "message",
          handleCustomerPaymentMethodsLoaded,
          `onCustomerPaymentMethodsLoaded-${componentType}`,
        )
      }
      let msg =
        [
          ("sendCustomerPaymentMethodsResponse", !disableSavedPaymentMethods->JSON.Encode.bool),
        ]->Dict.fromArray
      preMountLoaderIframeDiv->Window.iframePostMessage(msg)
    }

    !clientSecretReMatch
      ? manageErrorWarning(
          INVALID_FORMAT,
          ~dynamicStr="clientSecret is expected to be in format ******_secret_*****",
          ~logger,
          (),
        )
      : ()

    let setElementIframeRef = ref => {
      iframeRef->Array.push(ref)->ignore
      setIframeRef(ref)
    }
    let getElement = componentName => {
      savedPaymentElement->Dict.get(componentName)
    }
    let update = newOptions => {
      let newOptionsDict = newOptions->getDictFromJson
      switch newOptionsDict->Dict.get("locale") {
      | Some(val) => localOptions->Dict.set("locale", val)
      | None => ()
      }
      switch newOptionsDict->Dict.get("appearance") {
      | Some(val) => localOptions->Dict.set("appearance", val)
      | None => ()
      }
      switch newOptionsDict->Dict.get("clientSecret") {
      | Some(val) => localOptions->Dict.set("clientSecret", val)
      | None => ()
      }

      iframeRef->Array.forEach(iframe => {
        let message =
          [
            ("ElementsUpdate", true->JSON.Encode.bool),
            ("options", newOptionsDict->JSON.Encode.object),
          ]->Dict.fromArray
        iframe->Window.iframePostMessage(message)
      })
    }
    let fetchUpdates = () => {
      Promise.make((resolve, _) => {
        setTimeout(() => resolve(Dict.make()->JSON.Encode.object), 1000)->ignore
      })
    }

    let create = (componentType, newOptions) => {
      componentType == ""
        ? manageErrorWarning(REQUIRED_PARAMETER, ~dynamicStr="type", ~logger, ())
        : ()
      let otherElements = componentType->isOtherElements
      switch componentType {
      | "card"
      | "cardNumber"
      | "cardExpiry"
      | "cardCvc"
      | "paymentMethodCollect"
      | "googlePay"
      | "payPal"
      | "applePay"
      | "klarna"
      | "expressCheckout"
      | "paymentMethodsManagement"
      | "payment" => ()
      | str => manageErrorWarning(UNKNOWN_KEY, ~dynamicStr=`${str} type in create`, ~logger, ())
      }

      let mountPostMessage = (
        mountedIframeRef,
        selectorString,
        sdkHandleOneClickConfirmPayment,
      ) => {
        open Promise

        let widgetOptions =
          [
            ("clientSecret", clientSecret->JSON.Encode.string),
            ("appearance", appearance),
            ("locale", locale),
            ("loader", loader),
            ("fonts", fonts),
          ]->getJsonFromArrayOfJson
        let message =
          [
            (
              "paymentElementCreate",
              componentType->getIsComponentTypeForPaymentElementCreate->JSON.Encode.bool,
            ),
            ("otherElements", otherElements->JSON.Encode.bool),
            ("options", newOptions),
            ("componentType", componentType->JSON.Encode.string),
            ("paymentOptions", widgetOptions),
            ("iframeId", selectorString->JSON.Encode.string),
            ("publishableKey", publishableKey->JSON.Encode.string),
            ("endpoint", endpoint->JSON.Encode.string),
            ("sdkSessionId", sdkSessionId->JSON.Encode.string),
            ("blockConfirm", blockConfirm->JSON.Encode.bool),
            ("switchToCustomPod", switchToCustomPod->JSON.Encode.bool),
            ("sdkHandleOneClickConfirmPayment", sdkHandleOneClickConfirmPayment->JSON.Encode.bool),
            ("parentURL", "*"->JSON.Encode.string),
            ("analyticsMetadata", analyticsMetadata),
            ("launchTime", launchTime->JSON.Encode.float),
            ("customBackendUrl", customBackendUrl->JSON.Encode.string),
          ]->Dict.fromArray

        let wallets = PaymentType.getWallets(newOptions->getDictFromJson, "wallets", logger)

        let handleApplePayMounted = (event: Types.event) => {
          let json = event.data->anyTypeToJson
          let dict = json->getDictFromJson

          if dict->Dict.get("applePayMounted")->Option.isSome {
            if wallets.applePay === Auto {
              switch sessionForApplePay->Nullable.toOption {
              | Some(session) =>
                try {
                  if session.canMakePayments() {
                    let msg = [("applePayCanMakePayments", true->JSON.Encode.bool)]->Dict.fromArray
                    event.source->Window.sendPostMessage(msg)
                  } else {
                    Console.log("CANNOT MAKE PAYMENT USING APPLE PAY")
                    logger.setLogInfo(
                      ~value="CANNOT MAKE PAYMENT USING APPLE PAY",
                      ~eventName=APPLE_PAY_FLOW,
                      ~paymentMethod="APPLE_PAY",
                      ~logType=ERROR,
                      (),
                    )
                  }
                } catch {
                | exn => {
                    let exnString = exn->anyTypeToJson->JSON.stringify
                    Console.log("CANNOT MAKE PAYMENT USING APPLE PAY: " ++ exnString)
                    logger.setLogInfo(
                      ~value=exnString,
                      ~eventName=APPLE_PAY_FLOW,
                      ~paymentMethod="APPLE_PAY",
                      ~logType=ERROR,
                      (),
                    )
                  }
                }
              | None => ()
              }
            } else {
              logger.setLogInfo(
                ~value="ApplePay is set as 'never' by merchant",
                ~eventName=APPLE_PAY_FLOW,
                ~paymentMethod="APPLE_PAY",
                ~logType=INFO,
                (),
              )
            }
          }
        }

        // obsidian: the Google Pay and Apple Pay "third-party" flows ran only for the TrustPay
        // connector (its script and payment calls); removed with the connector.

        let handlePollStatusMessage = (ev: Types.event) => {
          let eventDataObject = ev.data->anyTypeToJson
          let headers = [("Content-Type", "application/json"), ("api-key", publishableKey)]
          switch eventDataObject->getOptionalJsonFromJson("confirmParams") {
          | Some(obj) => redirect := obj->getDictFromJson->getString("redirect", "if_required")
          | None => ()
          }
          switch eventDataObject->getOptionalJsonFromJson("poll_status") {
          | Some(val) => {
              handlePostMessage([
                ("fullscreen", true->JSON.Encode.bool),
                ("param", "paymentloader"->JSON.Encode.string),
                ("iframeId", selectorString->JSON.Encode.string),
              ])
              let dict = val->getDictFromJson
              let pollId = dict->getString("poll_id", "")
              let interval =
                dict->getString("delay_in_secs", "")->Int.fromString->Option.getOr(1) * 1000
              let count = dict->getString("frequency", "")->Int.fromString->Option.getOr(5)
              let url = dict->getString("return_url_with_query_params", "")
              PaymentHelpers.pollStatus(
                ~headers,
                ~switchToCustomPod,
                ~pollId,
                ~interval,
                ~count,
                ~returnUrl=url,
                ~logger,
              )
              ->then(_ => {
                PaymentHelpers.retrievePaymentIntent(
                  clientSecret,
                  headers,
                  ~optLogger=Some(logger),
                  ~switchToCustomPod,
                  ~isForceSync=true,
                )
                ->then(json => {
                  if redirect.contents === "always" {
                    let dict = json->JSON.Decode.object->Option.getOr(Dict.make())
                    let status = dict->getString("status", "")
                    let returnUrl = dict->getString("return_url", "")
                    Window.Location.replace(
                      `${returnUrl}?payment_intent_client_secret=${clientSecret}&status=${status}`,
                    )
                    resolve(JSON.Encode.null)
                  } else {
                    handlePostMessage([
                      ("fullscreen", false->JSON.Encode.bool),
                      ("submitSuccessful", true->JSON.Encode.bool),
                      ("data", json),
                    ])
                    resolve(json)
                  }
                })
                ->catch(err => {
                  if redirect.contents === "always" {
                    Window.Location.replace(url)
                  }
                  handlePostMessage([
                    ("submitSuccessful", false->JSON.Encode.bool),
                    ("error", err->Identity.anyTypeToJson),
                  ])
                  resolve(err->Identity.anyTypeToJson)
                })
                ->ignore
                ->resolve
              })
              ->catch(e => Console.log2("POLL_STATUS ERROR -", e)->resolve)
              ->ignore
            }
          | None => ()
          }

          switch eventDataObject->getOptionalJsonFromJson("openurl_if_required") {
          | Some(val) =>
            if redirect.contents === "always" {
              Window.Location.replace(val->JSON.Decode.string->Option.getOr(""))
              resolve(JSON.Encode.null)
            } else {
              PaymentHelpers.retrievePaymentIntent(
                clientSecret,
                headers,
                ~optLogger=Some(logger),
                ~switchToCustomPod,
                ~isForceSync=true,
              )
              ->then(json => {
                handlePostMessage([("submitSuccessful", true->JSON.Encode.bool), ("data", json)])
                resolve(json)
              })
              ->catch(err => {
                handlePostMessage([
                  ("submitSuccessful", false->JSON.Encode.bool),
                  ("error", err->Identity.anyTypeToJson),
                ])
                resolve(err->Identity.anyTypeToJson)
              })
              ->finally(_ => handlePostMessage([("fullscreen", false->JSON.Encode.bool)]))
            }->ignore

          | None => ()
          }
        }

        addSmartEventListener("message", handleApplePayMounted, "onApplePayMount")
        addSmartEventListener("message", handlePollStatusMessage, "onPollStatusMsg")

        let fetchSessionTokens = mountedIframeRef => {
          let handleSessionTokensLoaded = (event: Types.event) => {
            let json = event.data->Identity.anyTypeToJson
            let dict = json->getDictFromJson
            let sessionTokensData = dict->getString("data", "") === "session_tokens"
            if sessionTokensData {
              let json = dict->getJsonFromDict("response", JSON.Encode.null)

              {
                let sessionsArr =
                  json
                  ->JSON.Decode.object
                  ->Option.getOr(Dict.make())
                  ->SessionsType.getSessionsTokenJson("session_token")

                let applePayPresent = sessionsArr->Array.find(item => {
                  let x =
                    item
                    ->JSON.Decode.object
                    ->Belt.Option.flatMap(x => {
                      x->Dict.get("wallet_name")
                    })
                    ->Belt.Option.flatMap(JSON.Decode.string)
                    ->Option.getOr("")
                  x === "apple_pay" || x === "applepay"
                })
                if !(applePayPresent->Option.isSome) {
                  let msg =
                    [("applePaySessionObjNotPresent", true->JSON.Encode.bool)]->Dict.fromArray
                  mountedIframeRef->Window.iframePostMessage(msg)
                }
                let googlePayPresent = sessionsArr->Array.find(item => {
                  let x =
                    item
                    ->JSON.Decode.object
                    ->Belt.Option.flatMap(x => {
                      x->Dict.get("wallet_name")
                    })
                    ->Belt.Option.flatMap(JSON.Decode.string)
                    ->Option.getOr("")
                  x === "google_pay" || x === "googlepay"
                })

                (json, applePayPresent, googlePayPresent)->resolve
              }
              ->then(res => {
                let (json, applePayPresent, googlePayPresent) = res
                if (
                  componentType->getIsComponentTypeForPaymentElementCreate &&
                    applePayPresent->Option.isSome
                ) {
                  let handleApplePayMessages = (applePayEvent: Types.event) => {
                    let json = applePayEvent.data->Identity.anyTypeToJson
                    let dict = json->getDictFromJson
                    switch (
                      dict->Dict.get("applePayButtonClicked"),
                      dict->Dict.get("applePayPaymentRequest"),
                    ) {
                    | (Some(val), Some(paymentRequest)) =>
                      if val->JSON.Decode.bool->Option.getOr(false) {
                        let isDelayedSessionToken =
                          applePayPresent
                          ->Belt.Option.flatMap(JSON.Decode.object)
                          ->Option.getOr(Dict.make())
                          ->Dict.get("delayed_session_token")
                          ->Option.getOr(JSON.Encode.null)
                          ->JSON.Decode.bool
                          ->Option.getOr(false)
                        if !isDelayedSessionToken {
                          logger.setLogInfo(
                            ~value="Normal Session Token Flow",
                            ~eventName=APPLE_PAY_FLOW,
                            ~paymentMethod="APPLE_PAY",
                            (),
                          )

                          let callBackFunc = payment => {
                            let msg =
                              [
                                ("applePayProcessPayment", payment.token),
                                ("applePayBillingContact", payment.billingContact),
                                ("applePayShippingContact", payment.shippingContact),
                              ]->Dict.fromArray
                            applePayEvent.source->Window.sendPostMessage(msg)
                          }

                          ApplePayHelpers.startApplePaySession(
                            ~paymentRequest,
                            ~applePaySessionRef,
                            ~applePayPresent,
                            ~logger,
                            ~applePayEvent=Some(applePayEvent),
                            ~callBackFunc,
                          )
                        }
                      } else {
                        ()
                      }
                    | _ => ()
                    }
                  }

                  addSmartEventListener("message", handleApplePayMessages, "onApplePayMessages")
                }
                if (
                  componentType->getIsComponentTypeForPaymentElementCreate &&
                  googlePayPresent->Option.isSome &&
                  wallets.googlePay === Auto
                ) {
                  let dict = json->getDictFromJson
                  let sessionObj = SessionsType.itemToObjMapper(dict, Others)
                  let gPayToken = SessionsType.getPaymentSessionObj(sessionObj.sessionsToken, Gpay)

                  let tokenObj = switch gPayToken {
                  | OtherTokenOptional(optToken) => optToken
                  | _ => Some(SessionsType.defaultToken)
                  }

                  let gpayobj = switch tokenObj {
                  | Some(val) => val
                  | _ => SessionsType.defaultToken
                  }

                  let payRequest = GooglePayType.assign(
                    Dict.make()->JSON.Encode.object,
                    GooglePayType.baseRequest->Identity.anyTypeToJson,
                    {
                      "allowedPaymentMethods": gpayobj.allowed_payment_methods->arrayJsonToCamelCase,
                    }->Identity.anyTypeToJson,
                  )

                  try {
                    let gPayClient = GooglePayType.google(
                      {
                        "environment": publishableKey->String.startsWith("pk_prd_")
                          ? "PRODUCTION"
                          : "TEST",
                      }->Identity.anyTypeToJson,
                    )

                    gPayClient.isReadyToPay(payRequest)
                    ->then(res => {
                      let dict = res->getDictFromJson
                      let isReadyToPay = getBool(dict, "result", false)
                      let msg = [("isReadyToPay", isReadyToPay->JSON.Encode.bool)]->Dict.fromArray
                      mountedIframeRef->Window.iframePostMessage(msg)
                      resolve()
                    })
                    ->catch(err => {
                      logger.setLogInfo(
                        ~value=err->Identity.anyTypeToJson->JSON.stringify,
                        ~eventName=GOOGLE_PAY_FLOW,
                        ~paymentMethod="GOOGLE_PAY",
                        ~logType=DEBUG,
                        (),
                      )
                      resolve()
                    })
                    ->ignore

                    let handleGooglePayMessages = (event: Types.event) => {
                      let evJson = event.data->anyTypeToJson
                      let gpayClicked =
                        evJson
                        ->getOptionalJsonFromJson("GpayClicked")
                        ->getBoolFromJson(false)

                      let paymentDataRequest =
                        evJson
                        ->getOptionalJsonFromJson("GpayPaymentDataRequest")
                        ->Option.getOr(JSON.Encode.null)

                      if gpayClicked && paymentDataRequest !== JSON.Encode.null {
                        setTimeout(() => {
                          gPayClient.loadPaymentData(paymentDataRequest)
                          ->then(
                            json => {
                              let msg = [("gpayResponse", json->anyTypeToJson)]->Dict.fromArray
                              event.source->Window.sendPostMessage(msg)
                              let value = "Payment Data Filled: New Payment Method"
                              logger.setLogInfo(
                                ~value,
                                ~eventName=PAYMENT_DATA_FILLED,
                                ~paymentMethod="GOOGLE_PAY",
                                (),
                              )
                              resolve()
                            },
                          )
                          ->catch(
                            err => {
                              logger.setLogInfo(
                                ~value=err->anyTypeToJson->JSON.stringify,
                                ~eventName=GOOGLE_PAY_FLOW,
                                ~paymentMethod="GOOGLE_PAY",
                                ~logType=DEBUG,
                                (),
                              )

                              let msg = [("gpayError", err->anyTypeToJson)]->Dict.fromArray
                              event.source->Window.sendPostMessage(msg)
                              resolve()
                            },
                          )
                          ->ignore
                        }, 0)->ignore
                      }
                    }
                    addSmartEventListener("message", handleGooglePayMessages, "onGooglePayMessages")
                  } catch {
                  | _ => Console.log("Error loading Gpay")
                  }
                } else if wallets.googlePay === Never {
                  logger.setLogInfo(
                    ~value="GooglePay is set as never by merchant",
                    ~eventName=GOOGLE_PAY_FLOW,
                    ~paymentMethod="GOOGLE_PAY",
                    ~logType=INFO,
                    (),
                  )
                }

                json->resolve
              })
              ->then(json => {
                let msg = [("sessions", json)]->Dict.fromArray
                mountedIframeRef->Window.iframePostMessage(msg)
                json->resolve
              })
              ->ignore
            }
          }
          let msg = [("sendSessionTokensResponse", true->JSON.Encode.bool)]->Dict.fromArray
          addSmartEventListener(
            "message",
            handleSessionTokensLoaded,
            `onSessionTokensLoaded-${componentType}`,
          )
          preMountLoaderIframeDiv->Window.iframePostMessage(msg)
        }
        preMountLoaderMountedPromise
        ->then(_ => {
          fetchPaymentsList(mountedIframeRef, componentType)
          let disableSavedPaymentMethods =
            newOptions
            ->getDictFromJson
            ->getBool("displaySavedPaymentMethods", true) &&
              !(spmComponents->Array.includes(componentType))->not
          fetchCustomerPaymentMethods(mountedIframeRef, disableSavedPaymentMethods, componentType)
          fetchSessionTokens(mountedIframeRef)
          resolve()
        })
        ->ignore
        mountedIframeRef->Window.iframePostMessage(message)
      }

      let paymentElement = LoaderPaymentElement.make(
        componentType,
        newOptions,
        setElementIframeRef,
        iframeRef,
        mountPostMessage,
      )
      savedPaymentElement->Dict.set(componentType, paymentElement)
      paymentElement
    }
    {
      getElement,
      update,
      fetchUpdates,
      create,
    }
  } catch {
  | e => {
      Sentry.captureException(e)
      defaultElement
    }
  }
}
