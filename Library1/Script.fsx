#r @"..\Library1\bin\Release\Library1.dll"
open HierarchicalOperationViewModels
open System.ComponentModel

let x = 
    {   Parent = None
        Items  = []
        Name = ""
        Action = None
        State = {  Checked = BoolFalse; HandleChecked = false}
        Perform  = null
        IsFreezed = false}
let a = x |> box :?> INotifyPropertyChanged
a.PropertyChanged.AddHandler(fun _ e -> printfn "property changed: %A" e.PropertyName )
x.IsFreezed <- true