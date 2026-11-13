type wallet = GPayWallet | PaypalWallet | ApplePayWallet | KlarnaWallet | NONE
let paymentMode = str => {
  switch str {
  | "gpay"
  | "google_pay" =>
    GPayWallet
  | "paypal" => PaypalWallet
  | "applepay"
  | "apple_pay" =>
    ApplePayWallet
  | "klarna" => KlarnaWallet
  | _ => NONE
  }
}

module WalletsSaveDetailsText = {
  @react.component
  let make = (~paymentType) => {
    open RecoilAtoms
    let {isGooglePay, isApplePay, isPaypal} = Recoil.useRecoilValueFromAtom(
      areOneClickWalletsRendered,
    )
    let {localeString} = Recoil.useRecoilValueFromAtom(configAtom)
    let isGuestCustomer = UtilityHooks.useIsGuestCustomer()

    <RenderIf
      condition={PaymentUtils.isAppendingCustomerAcceptance(~isGuestCustomer, ~paymentType) &&
      (isGooglePay || isApplePay || isPaypal)}>
      <div className="flex items-center text-xs mt-2">
        <Icon name="lock" size=10 className="mr-1" />
        <em className="text-left text-gray-400">
          {localeString.saveWalletDetails->React.string}
        </em>
      </div>
    </RenderIf>
  }
}

@react.component
let make = (~sessions, ~walletOptions) => {
  open SessionsType
  let dict = sessions->Utils.getDictFromJson
  let paymentMethodListValue = Recoil.useRecoilValueFromAtom(PaymentUtils.paymentMethodListValue)

  let sessionObj = React.useMemo(() => itemToObjMapper(dict, Others), [dict])
  let gPayToken = getPaymentSessionObj(sessionObj.sessionsToken, Gpay)
  let applePaySessionObj = itemToObjMapper(dict, ApplePayObject)
  let applePayToken = getPaymentSessionObj(applePaySessionObj.sessionsToken, ApplePay)

  let googlePayThirdPartySessionObj = itemToObjMapper(dict, GooglePayThirdPartyObject)
  let googlePayThirdPartyToken = getPaymentSessionObj(
    googlePayThirdPartySessionObj.sessionsToken,
    Gpay,
  )

  let {clientSecret} = Recoil.useRecoilValueFromAtom(RecoilAtoms.keys)

  <div className="flex flex-col gap-2 h-auto w-full">
    {walletOptions
    ->Array.mapWithIndex((item, i) => {
      <ErrorBoundary
        level={ErrorBoundary.RequestButton} key={`${item}-${i->Int.toString}-request-button`}>
        <React.Suspense fallback={<WalletShimmer />} key={i->Int.toString}>
          {switch clientSecret {
          | Some(_) =>
            switch item->paymentMode {
            | GPayWallet =>
              <SessionPaymentWrapper type_={Wallet}>
                {switch gPayToken {
                | OtherTokenOptional(optToken) =>
                  switch googlePayThirdPartyToken {
                  | GooglePayThirdPartyTokenOptional(googlePayThirdPartyOptToken) =>
                    <GPayLazy
                      sessionObj=optToken thirdPartySessionObj=googlePayThirdPartyOptToken
                    />
                  | _ => <GPayLazy sessionObj=optToken thirdPartySessionObj=None />
                  }
                | _ => React.null
                }}
              </SessionPaymentWrapper>
            | PaypalWallet =>
              // obsidian: always the redirect button; PayPal's JS SDK (used with a PayPal or Braintree
              // connector session) was removed with the connectors.
              <SessionPaymentWrapper type_={Wallet}>
                <PayPalLazy />
              </SessionPaymentWrapper>
            | ApplePayWallet =>
              switch applePayToken {
              | ApplePayTokenOptional(optToken) => <ApplePayLazy sessionObj=optToken />
              | _ => React.null
              }
            // obsidian: the Klarna button was Klarna's JS SDK on a Klarna connector session; removed
            // with the connector.
            | KlarnaWallet => React.null

            | NONE => React.null
            }
          | None => React.null
          }}
        </React.Suspense>
      </ErrorBoundary>
    })
    ->React.array}
    <Surcharge paymentMethod="wallet" paymentMethodType="google_pay" isForWallets=true />
    <WalletsSaveDetailsText paymentType=paymentMethodListValue.payment_type />
  </div>
}
