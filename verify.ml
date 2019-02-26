open Lang

let verify premises conclusion =
  Some [ Judgement { statement = conclusion
                   ; refs = premises
                   ; rule = PO }
       ]


