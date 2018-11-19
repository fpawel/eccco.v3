[<AutoOpen>]
module Utils

open System

let BoolTrue = Nullable<bool>(true)
let BoolFalse = Nullable<bool>(false)
let BoolNone = Nullable<bool>()

type ICommand = Windows.Input.ICommand
type NotifyCollectionChangedAction = Collections.Specialized.NotifyCollectionChangedAction


let private _event_commands = Event<_, _>().Publish
let wpfCommnad canExecute action =    
    {   new System.Windows.Input.ICommand with
            member this.CanExecute _ = 
                canExecute()
            member this.Execute _ = 
                action()
            member this.add_CanExecuteChanged(handler) = _event_commands.AddHandler(handler)
            member this.remove_CanExecuteChanged(handler) = _event_commands.RemoveHandler(handler) }

let wpfCommnad1 f =
    wpfCommnad (fun() -> true) f

let subscribePropertyChanged x f =
    (x |> box :?> ComponentModel.INotifyPropertyChanged).PropertyChanged.Add f

let subscribePropertyChangedAction x f =
    let f = ComponentModel.PropertyChangedEventHandler(f)
    (x |> box :?> ComponentModel.INotifyPropertyChanged).PropertyChanged.AddHandler f
    f

let subscribePropertyChangeHandler x f =
    (x |> box :?> ComponentModel.INotifyPropertyChanged).PropertyChanged.AddHandler f

let removePropertyChangedAction x f =
    (x |> box :?> ComponentModel.INotifyPropertyChanged).PropertyChanged.RemoveHandler f
