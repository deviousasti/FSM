namespace FSM

module StateActor = 
    
    type Message<'a, 'b> = Move of 'a * AsyncReplyChannel<'b> | Die

    let fromState initial = 
        MailboxProcessor.Start(fun inbox -> 
        let rec loop state = async { 
            let! msg = inbox.Receive()
            match msg with 
               | Die -> return ()
               | Move(v, channel) -> let s, v = States.eval state v 
                                     channel.Reply(v)
                                     return! loop s
        }
        loop initial)