let switchToInteg = false
let isLocal = false
let sdkDomainUrl = `${GlobalVars.sdkUrl}${GlobalVars.repoPublicPath}`

let apiEndPoint: ref<option<string>> = ref(None)

let setApiEndPoint = str => {
  apiEndPoint := Some(str)
}

// obsidian: always our own backend. Upstream sent test-key (pk_snd_) calls from a prod build to
// the Hyperswitch-hosted sandbox; a self-hosted gateway must never send shopper data there.
let getApiEndPoint = (~publishableKey as _="", ~isConfirmCall=false, ()) => {
  switch apiEndPoint.contents {
  | Some(str) => str
  | None => isConfirmCall ? GlobalVars.confirmEndPoint : GlobalVars.backendEndPoint
  }
}

let addCustomPodHeader = (arr: array<(string, string)>, ~switchToCustomPod=?, ()) => {
  let customPod = switch switchToCustomPod {
  | Some(val) => val
  | None => false
  }
  if customPod {
    arr->Array.concat([("x-feature", "router-custom-dbd")])->Dict.fromArray
  } else {
    arr->Dict.fromArray
  }
}
