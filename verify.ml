let verify premises conclusion =
  Some [ Judgement.t { statement = List.nth premises 0
                     ; refs = premises
                     ; rule = PO }
       ]


