@react.component
let make = (~className="pt-4") => {
  let {branding} = Recoil.useRecoilValueFromAtom(RecoilAtoms.optionAtom)
  <RenderIf condition={branding == Auto}>
    <div className={`text-xs text-center w-full flex justify-center ${className}`}>
      // obsidian: Obsidian Pay instead of "powered by hyperswitch"
      <Icon size=16 width=148 name="powered-by-obsidian" />
    </div>
  </RenderIf>
}
