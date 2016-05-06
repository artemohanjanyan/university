module Statement.Proofs
    ( baseProof
    , negProof
    , excludedMiddle
    , removalEnding
    ) where

import Statement.Types

negProof :: Bool -> (Bool, Proof)
negProof False = (True, Proof [stmt] (stmt) [stmt])
  where
    stmt = Neg $ Var "A"

negProof True = (False, Proof [Var "A"] (Neg $ Neg $ Var "A") $
    map parseStmt $
    [ "A -> !A -> A"
    , "A"
    , "!A -> A"
    , "!A -> (!A -> !A) -> !A"
    , "!A -> !A -> !A"
    , "(!A -> (!A -> !A)) -> (!A -> (!A -> !A) -> !A) -> (!A -> !A)"
    , "(!A -> (!A -> !A) -> !A) -> (!A -> !A)"
    , "(!A -> !A)"
    , "(!A -> A) -> (!A -> !A) -> !!A"
    , "(!A -> !A) -> !!A"
    , "!!A"
    ])

baseProof :: Oprtr -> Bool -> Bool -> (Bool, Proof)
baseProof oprtr left right = (isNotNeg target, Proof
        [valued left (Var "A"), valued right (Var "B")]
        target
        parsed)
  where
    parsed = map parseStmt $ baseProof' oprtr left right
    valued True stmt = stmt
    valued False stmt = Neg stmt
    target = last parsed
    isNotNeg (Neg _) = False
    isNotNeg _       = True

baseProof' :: Oprtr -> Bool -> Bool -> [String]
baseProof' Impl False False =
    [ "(A->!B->A)->A->(A->!B->A)"
    , "(A->!B->A)"
    , "A->(A->!B->A)"
    , "A->A->A"
    , "(A->A->A)->(A->(A->A)->A)->(A->A)"
    , "(A->(A->A)->A)->(A->A)"
    , "A->(A->A)->A"
    , "A->A"
    , "(A->A)->(A->A->(!B->A))->(A->(!B->A))"
    , "(A->A->!B->A)->(A->!B->A)"
    , "A->!B->A"
    , "(!A->!B->!A)->A->(!A->!B->!A)"
    , "!A->!B->!A"
    , "A->!A->!B->!A"
    , "!A->A->!A"
    , "!A"
    , "A->!A"
    , "(A->!A)->(A->!A->!B->!A)->(A->!B->!A)"
    , "(A->!A->!B->!A)->(A->!B->!A)"
    , "A->!B->!A"
    , "((!B->A)->(!B->!A)->!!B)->A->((!B->A)->(!B->!A)->!!B)"
    , "(!B->A)->(!B->!A)->!!B"
    , "A->((!B->A)->(!B->!A)->!!B)"
    , "(A->(!B->A))->(A->(!B->A)->((!B->!A)->!!B))->(A->((!B->!A)->!!B))"
    , "((A->((!B->A)->((!B->!A)->!!B)))->(A->((!B->!A)->!!B)))"
    , "A->((!B->!A)->!!B)"
    , "(A->(!B->!A))->(A->(!B->!A)->!!B)->(A->!!B)"
    , "((A->((!B->!A)->!!B))->(A->!!B))"
    , "A->!!B"
    , "(!!B->B)->A->(!!B->B)"
    , "!!B->B"
    , "A->!!B->B"
    , "(A->!!B)->(A->!!B->B)->(A->B)"
    , "((A->!!B->B)->(A->B))"
    , "A->B"
    ]

baseProof' Impl False True =
    [ "B->A->B"
    , "B"
    , "A->B"
    ]

baseProof' Impl True False =
    [ "((A->B)->B)->((A->B)->!B)->!(A->B)"
    , "((A->B)->A)->((A->B)->(A->B))->((A->B)->B)"
    , "A"
    , "A->(A->B)->A"
    , "(A->B)->A"
    , "((A->B)->(A->B))->((A->B)->B)"
    , "((A->B)->(A->B)->(A->B))"
    , "((A->B)->(A->B)->(A->B))->((A->B)->((A->B)->(A->B))->(A->B))->((A->B)->(A->B))"
    , "((A->B)->((A->B)->(A->B))->(A->B))->((A->B)->(A->B))"
    , "((A->B)->((A->B)->(A->B))->(A->B))"
    , "(A->B)->(A->B)"
    , "(A->B)->B"
    , "!B->(A->B)->!B"
    , "!B->(A->B)->!B"
    , "!B"
    , "(A->B)->!B"
    , "((A->B)->!B)->!(A->B)"
    , "!(A->B)"
    ]

baseProof' Impl True True =
    [ "B->A->B"
    , "B"
    , "A->B"
    ]

baseProof' Disj False False =
    [ "!A"
    , "(A|B->(A))->(A|B->!(A))->!(A|B)"
    , "(A->A)->(B->A)->(A|B->A)"
    , "A->A->A"
    , "(A->A->A)->(A->(A->A)->A)->(A->A)"
    , "(A->(A->A)->A)->(A->A)"
    , "A->(A->A)->A"
    , "A->A"
    , "(B->A)->(A|B->A)"
    , "(B->!A->B)->B->(B->!A->B)"
    , "(B->!A->B)"
    , "B->(B->!A->B)"
    , "B->B->B"
    , "(B->B->B)->(B->(B->B)->B)->(B->B)"
    , "(B->(B->B)->B)->(B->B)"
    , "B->(B->B)->B"
    , "B->B"
    , "(B->B)->(B->B->(!A->B))->(B->(!A->B))"
    , "(B->B->!A->B)->(B->!A->B)"
    , "B->!A->B"
    , "(!B->!A->!B)->B->(!B->!A->!B)"
    , "!B->!A->!B"
    , "B->!B->!A->!B"
    , "!B->B->!B"
    , "!B"
    , "B->!B"
    , "(B->!B)->(B->!B->!A->!B)->(B->!A->!B)"
    , "(B->!B->!A->!B)->(B->!A->!B)"
    , "B->!A->!B"
    , "((!A->B)->(!A->!B)->!!A)->B->((!A->B)->(!A->!B)->!!A)"
    , "(!A->B)->(!A->!B)->!!A"
    , "B->((!A->B)->(!A->!B)->!!A)"
    , "(B->(!A->B))->(B->(!A->B)->((!A->!B)->!!A))->(B->((!A->!B)->!!A))"
    , "((B->((!A->B)->((!A->!B)->!!A)))->(B->((!A->!B)->!!A)))"
    , "B->((!A->!B)->!!A)"
    , "(B->(!A->!B))->(B->(!A->!B)->!!A)->(B->!!A)"
    , "((B->((!A->!B)->!!A))->(B->!!A))"
    , "B->!!A"
    , "(!!A->A)->B->(!!A->A)"
    , "!!A->A"
    , "B->!!A->A"
    , "(B->!!A)->(B->!!A->A)->(B->A)"
    , "((B->!!A->A)->(B->A))"
    , "B->A"
    , "A|B->A"
    , "(A|B->!A)->!(A|B)"
    , "!A->A|B->!A"
    , "A|B->!A"
    , "!(A|B)"
    ]

baseProof' Disj False True =
    [ "B->A|B"
    , "B"
    , "A|B"
    ]

baseProof' Disj True False =
    [ "A->A|B"
    , "A"
    , "A|B"
    ]

baseProof' Disj True True =
    [ "A->A|B"
    , "A"
    , "A|B"
    ]

baseProof' Conj False False =
    [ "((A&B)->A)->((A&B)->!A)->!(A&B)"
    , "(A&B)->A "
    , "(A&B->!A)->!(A&B)"
    , "!A->A&B->!A"
    , "!A"
    , "A&B->!A"
    , "!(A&B)"
    ]

baseProof' Conj False True =
    [ "((A&B)->A)->((A&B)->!A)->!(A&B)"
    , "(A&B)->A"
    , "(A&B->!A)->!(A&B)"
    , "!A->A&B->!A"
    , "!A"
    , "A&B->!A"
    , "!(A&B)"
    ]

baseProof' Conj True False =
    [ "((A&B)->B)->((A&B)->!B)->!(A&B)"
    , "(A&B)->B"
    , "(A&B->!B)->!(A&B)"
    , "!B->A&B->!B"
    , "!B"
    , "A&B->!B"
    , "!(A&B)"
    ]

baseProof' Conj True True =
    [ "A->B->A&B"
    , "A"
    , "B->A&B"
    , "B"
    , "A&B"
    ]

excludedMiddle = Proof [] (parseStmt "A|!A") $ map parseStmt
    [ "A->A|!A"
    , "(((A->(A|!A))->(A->!(A|!A))->!A)->!(A|!A)->((A->(A|!A))->(A->!(A|!A))->!A))->(A->(A|!A))->(((A->(A|!A))->(A->!(A|!A))->!A)->!(A|!A)->((A->(A|!A))->(A->!(A|!A))->!A))"
    , "(((A->(A|!A))->(A->!(A|!A))->!A)->!(A|!A)->((A->(A|!A))->(A->!(A|!A))->!A))"
    , "(A->(A|!A))->(((A->(A|!A))->(A->!(A|!A))->!A)->!(A|!A)->((A->(A|!A))->(A->!(A|!A))->!A))"
    , "(((A->(A|!A))->(A->!(A|!A))->!A))->(A->(A|!A))->(((A->(A|!A))->(A->!(A|!A))->!A))"
    , "(((A->(A|!A))->(A->!(A|!A))->!A))"
    , "(A->(A|!A))->(((A->(A|!A))->(A->!(A|!A))->!A))"
    , "((A->(A|!A))->((A->(A|!A))->((A->!(A|!A))->!A)))->((A->(A|!A))->((A->(A|!A))->((A->!(A|!A))->!A))->(!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A))))->((A->(A|!A))->(!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A))))"
    , "(((A->(A|!A))->(((A->(A|!A))->((A->!(A|!A))->!A))->(!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))))->((A->(A|!A))->(!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))))"
    , "(A->(A|!A))->(!(A|!A)->((A->(A|!A))->(A->!(A|!A))->!A))"
    , "((A->(A|!A))->!(A|!A)->(A->(A|!A)))->(A->(A|!A))->((A->(A|!A))->!(A|!A)->(A->(A|!A)))"
    , "((A->(A|!A))->!(A|!A)->(A->(A|!A)))"
    , "(A->(A|!A))->((A->(A|!A))->!(A|!A)->(A->(A|!A)))"
    , "(A->(A|!A))->(A->(A|!A))->(A->(A|!A))"
    , "((A->(A|!A))->(A->(A|!A))->(A->(A|!A)))->((A->(A|!A))->((A->(A|!A))->(A->(A|!A)))->(A->(A|!A)))->((A->(A|!A))->(A->(A|!A)))"
    , "((A->(A|!A))->((A->(A|!A))->(A->(A|!A)))->(A->(A|!A)))->((A->(A|!A))->(A->(A|!A)))"
    , "(A->(A|!A))->((A->(A|!A))->(A->(A|!A)))->(A->(A|!A))"
    , "(A->(A|!A))->(A->(A|!A))"
    , "((A->(A|!A))->(A->(A|!A)))->((A->(A|!A))->(A->(A|!A))->(!(A|!A)->(A->(A|!A))))->((A->(A|!A))->(!(A|!A)->(A->(A|!A))))"
    , "(((A->(A|!A))->((A->(A|!A))->(!(A|!A)->(A->(A|!A)))))->((A->(A|!A))->(!(A|!A)->(A->(A|!A)))))"
    , "(A->(A|!A))->(!(A|!A)->(A->(A|!A)))"
    , "((!(A|!A)->(A->(A|!A)))->(!(A|!A)->(A->(A|!A))->((A->!(A|!A))->!A))->(!(A|!A)->((A->!(A|!A))->!A)))->(A->(A|!A))->((!(A|!A)->(A->(A|!A)))->(!(A|!A)->(A->(A|!A))->((A->!(A|!A))->!A))->(!(A|!A)->((A->!(A|!A))->!A)))"
    , "((!(A|!A)->(A->(A|!A)))->(!(A|!A)->(A->(A|!A))->((A->!(A|!A))->!A))->(!(A|!A)->((A->!(A|!A))->!A)))"
    , "(A->(A|!A))->((!(A|!A)->(A->(A|!A)))->(!(A|!A)->(A->(A|!A))->((A->!(A|!A))->!A))->(!(A|!A)->((A->!(A|!A))->!A)))"
    , "((A->(A|!A))->(!(A|!A)->(A->(A|!A))))->((A->(A|!A))->(!(A|!A)->(A->(A|!A)))->((!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))->(!(A|!A)->((A->!(A|!A))->!A))))->((A->(A|!A))->((!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))->(!(A|!A)->((A->!(A|!A))->!A))))"
    , "(((A->(A|!A))->((!(A|!A)->(A->(A|!A)))->((!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))->(!(A|!A)->((A->!(A|!A))->!A)))))->((A->(A|!A))->((!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))->(!(A|!A)->((A->!(A|!A))->!A)))))"
    , "(A->(A|!A))->(((!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))->(!(A|!A)->((A->!(A|!A))->!A))))"
    , "((A->(A|!A))->(!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A))))->((A->(A|!A))->(!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))->(!(A|!A)->((A->!(A|!A))->!A)))->((A->(A|!A))->(!(A|!A)->((A->!(A|!A))->!A)))"
    , "(((A->(A|!A))->((!(A|!A)->((A->(A|!A))->((A->!(A|!A))->!A)))->(!(A|!A)->((A->!(A|!A))->!A))))->((A->(A|!A))->(!(A|!A)->((A->!(A|!A))->!A))))"
    , "(A->(A|!A))->(!(A|!A)->((A->!(A|!A))->!A))"
    , "((!(A|!A)->A->!(A|!A))->!(A|!A)->(!(A|!A)->A->!(A|!A)))->(A->(A|!A))->((!(A|!A)->A->!(A|!A))->!(A|!A)->(!(A|!A)->A->!(A|!A)))"
    , "((!(A|!A)->A->!(A|!A))->!(A|!A)->(!(A|!A)->A->!(A|!A)))"
    , "(A->(A|!A))->((!(A|!A)->A->!(A|!A))->!(A|!A)->(!(A|!A)->A->!(A|!A)))"
    , "((!(A|!A)->A->!(A|!A)))->(A->(A|!A))->((!(A|!A)->A->!(A|!A)))"
    , "((!(A|!A)->A->!(A|!A)))"
    , "(A->(A|!A))->((!(A|!A)->A->!(A|!A)))"
    , "((A->(A|!A))->(!(A|!A)->(A->!(A|!A))))->((A->(A|!A))->(!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(!(A|!A)->(A->!(A|!A)))))->((A->(A|!A))->(!(A|!A)->(!(A|!A)->(A->!(A|!A)))))"
    , "(((A->(A|!A))->((!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(!(A|!A)->(A->!(A|!A))))))->((A->(A|!A))->(!(A|!A)->(!(A|!A)->(A->!(A|!A))))))"
    , "(A->(A|!A))->(!(A|!A)->(!(A|!A)->A->!(A|!A)))"
    , "(!(A|!A)->!(A|!A)->!(A|!A))->(A->(A|!A))->(!(A|!A)->!(A|!A)->!(A|!A))"
    , "(!(A|!A)->!(A|!A)->!(A|!A))"
    , "(A->(A|!A))->(!(A|!A)->!(A|!A)->!(A|!A))"
    , "((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))->(A->(A|!A))->((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "(A->(A|!A))->((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "((A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))))->((A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A)))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A))))->((A->(A|!A))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A))))"
    , "(((A->(A|!A))->((!(A|!A)->(!(A|!A)->!(A|!A)))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A)))))->((A->(A|!A))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A)))))"
    , "(A->(A|!A))->((!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))"
    , "(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))"
    , "(A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))"
    , "((A->(A|!A))->(!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A))))->((A->(A|!A))->(!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A)))->((A->(A|!A))->(!(A|!A)->!(A|!A)))"
    , "(((A->(A|!A))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A))))->((A->(A|!A))->(!(A|!A)->!(A|!A))))"
    , "(A->(A|!A))->(!(A|!A)->!(A|!A))"
    , "((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))))->(A->(A|!A))->((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))))"
    , "((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))))"
    , "(A->(A|!A))->((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))))"
    , "((A->(A|!A))->(!(A|!A)->!(A|!A)))->((A->(A|!A))->(!(A|!A)->!(A|!A))->((!(A|!A)->(!(A|!A)->(A->!(A|!A))))->(!(A|!A)->(A->!(A|!A)))))->((A->(A|!A))->((!(A|!A)->(!(A|!A)->(A->!(A|!A))))->(!(A|!A)->(A->!(A|!A)))))"
    , "(((A->(A|!A))->((!(A|!A)->!(A|!A))->((!(A|!A)->(!(A|!A)->(A->!(A|!A))))->(!(A|!A)->(A->!(A|!A))))))->((A->(A|!A))->((!(A|!A)->(!(A|!A)->(A->!(A|!A))))->(!(A|!A)->(A->!(A|!A))))))"
    , "(A->(A|!A))->(((!(A|!A)->(!(A|!A)->(A->!(A|!A))))->(!(A|!A)->(A->!(A|!A)))))"
    , "(!(A|!A)->(A->!(A|!A)))->(A->(A|!A))->(!(A|!A)->(A->!(A|!A)))"
    , "(!(A|!A)->(A->!(A|!A)))"
    , "(A->(A|!A))->(!(A|!A)->(A->!(A|!A)))"
    , "((!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))->!A)->(!(A|!A)->!A))->(A->(A|!A))->((!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))->!A)->(!(A|!A)->!A))"
    , "((!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))->!A)->(!(A|!A)->!A))"
    , "(A->(A|!A))->((!(A|!A)->(A->!(A|!A)))->(!(A|!A)->(A->!(A|!A))->!A)->(!(A|!A)->!A))"
    , "((A->(A|!A))->(!(A|!A)->(A->!(A|!A))))->((A->(A|!A))->(!(A|!A)->(A->!(A|!A)))->((!(A|!A)->((A->!(A|!A))->!A))->(!(A|!A)->!A)))->((A->(A|!A))->((!(A|!A)->((A->!(A|!A))->!A))->(!(A|!A)->!A)))"
    , "(((A->(A|!A))->((!(A|!A)->(A->!(A|!A)))->((!(A|!A)->((A->!(A|!A))->!A))->(!(A|!A)->!A))))->((A->(A|!A))->((!(A|!A)->((A->!(A|!A))->!A))->(!(A|!A)->!A))))"
    , "(A->(A|!A))->(((!(A|!A)->((A->!(A|!A))->!A))->(!(A|!A)->!A)))"
    , "((A->(A|!A))->(!(A|!A)->((A->!(A|!A))->!A)))->((A->(A|!A))->(!(A|!A)->((A->!(A|!A))->!A))->(!(A|!A)->!A))->((A->(A|!A))->(!(A|!A)->!A))"
    , "(((A->(A|!A))->((!(A|!A)->((A->!(A|!A))->!A))->(!(A|!A)->!A)))->((A->(A|!A))->(!(A|!A)->!A)))"
    , "(A->(A|!A))->(!(A|!A)->(!A))"
    , "!(A|!A)->!A"
    , "!A->A|!A"
    , "(((!A->(A|!A))->(!A->!(A|!A))->!!A)->!(A|!A)->((!A->(A|!A))->(!A->!(A|!A))->!!A))->(!A->(A|!A))->(((!A->(A|!A))->(!A->!(A|!A))->!!A)->!(A|!A)->((!A->(A|!A))->(!A->!(A|!A))->!!A))"
    , "(((!A->(A|!A))->(!A->!(A|!A))->!!A)->!(A|!A)->((!A->(A|!A))->(!A->!(A|!A))->!!A))"
    , "(!A->(A|!A))->(((!A->(A|!A))->(!A->!(A|!A))->!!A)->!(A|!A)->((!A->(A|!A))->(!A->!(A|!A))->!!A))"
    , "(((!A->(A|!A))->(!A->!(A|!A))->!!A))->(!A->(A|!A))->(((!A->(A|!A))->(!A->!(A|!A))->!!A))"
    , "(((!A->(A|!A))->(!A->!(A|!A))->!!A))"
    , "(!A->(A|!A))->(((!A->(A|!A))->(!A->!(A|!A))->!!A))"
    , "((!A->(A|!A))->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->((!A->(A|!A))->((!A->(A|!A))->((!A->!(A|!A))->!!A))->(!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A))))->((!A->(A|!A))->(!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A))))"
    , "(((!A->(A|!A))->(((!A->(A|!A))->((!A->!(A|!A))->!!A))->(!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))))->((!A->(A|!A))->(!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))))"
    , "(!A->(A|!A))->(!(A|!A)->((!A->(A|!A))->(!A->!(A|!A))->!!A))"
    , "((!A->(A|!A))->!(A|!A)->(!A->(A|!A)))->(!A->(A|!A))->((!A->(A|!A))->!(A|!A)->(!A->(A|!A)))"
    , "((!A->(A|!A))->!(A|!A)->(!A->(A|!A)))"
    , "(!A->(A|!A))->((!A->(A|!A))->!(A|!A)->(!A->(A|!A)))"
    , "(!A->(A|!A))->(!A->(A|!A))->(!A->(A|!A))"
    , "((!A->(A|!A))->(!A->(A|!A))->(!A->(A|!A)))->((!A->(A|!A))->((!A->(A|!A))->(!A->(A|!A)))->(!A->(A|!A)))->((!A->(A|!A))->(!A->(A|!A)))"
    , "((!A->(A|!A))->((!A->(A|!A))->(!A->(A|!A)))->(!A->(A|!A)))->((!A->(A|!A))->(!A->(A|!A)))"
    , "(!A->(A|!A))->((!A->(A|!A))->(!A->(A|!A)))->(!A->(A|!A))"
    , "(!A->(A|!A))->(!A->(A|!A))"
    , "((!A->(A|!A))->(!A->(A|!A)))->((!A->(A|!A))->(!A->(A|!A))->(!(A|!A)->(!A->(A|!A))))->((!A->(A|!A))->(!(A|!A)->(!A->(A|!A))))"
    , "(((!A->(A|!A))->((!A->(A|!A))->(!(A|!A)->(!A->(A|!A)))))->((!A->(A|!A))->(!(A|!A)->(!A->(A|!A)))))"
    , "(!A->(A|!A))->(!(A|!A)->(!A->(A|!A)))"
    , "((!(A|!A)->(!A->(A|!A)))->(!(A|!A)->(!A->(A|!A))->((!A->!(A|!A))->!!A))->(!(A|!A)->((!A->!(A|!A))->!!A)))->(!A->(A|!A))->((!(A|!A)->(!A->(A|!A)))->(!(A|!A)->(!A->(A|!A))->((!A->!(A|!A))->!!A))->(!(A|!A)->((!A->!(A|!A))->!!A)))"
    , "((!(A|!A)->(!A->(A|!A)))->(!(A|!A)->(!A->(A|!A))->((!A->!(A|!A))->!!A))->(!(A|!A)->((!A->!(A|!A))->!!A)))"
    , "(!A->(A|!A))->((!(A|!A)->(!A->(A|!A)))->(!(A|!A)->(!A->(A|!A))->((!A->!(A|!A))->!!A))->(!(A|!A)->((!A->!(A|!A))->!!A)))"
    , "((!A->(A|!A))->(!(A|!A)->(!A->(A|!A))))->((!A->(A|!A))->(!(A|!A)->(!A->(A|!A)))->((!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->(!(A|!A)->((!A->!(A|!A))->!!A))))->((!A->(A|!A))->((!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->(!(A|!A)->((!A->!(A|!A))->!!A))))"
    , "(((!A->(A|!A))->((!(A|!A)->(!A->(A|!A)))->((!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->(!(A|!A)->((!A->!(A|!A))->!!A)))))->((!A->(A|!A))->((!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->(!(A|!A)->((!A->!(A|!A))->!!A)))))"
    , "(!A->(A|!A))->(((!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->(!(A|!A)->((!A->!(A|!A))->!!A))))"
    , "((!A->(A|!A))->(!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A))))->((!A->(A|!A))->(!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->(!(A|!A)->((!A->!(A|!A))->!!A)))->((!A->(A|!A))->(!(A|!A)->((!A->!(A|!A))->!!A)))"
    , "(((!A->(A|!A))->((!(A|!A)->((!A->(A|!A))->((!A->!(A|!A))->!!A)))->(!(A|!A)->((!A->!(A|!A))->!!A))))->((!A->(A|!A))->(!(A|!A)->((!A->!(A|!A))->!!A))))"
    , "(!A->(A|!A))->(!(A|!A)->((!A->!(A|!A))->!!A))"
    , "((!(A|!A)->!A->!(A|!A))->!(A|!A)->(!(A|!A)->!A->!(A|!A)))->(!A->(A|!A))->((!(A|!A)->!A->!(A|!A))->!(A|!A)->(!(A|!A)->!A->!(A|!A)))"
    , "((!(A|!A)->!A->!(A|!A))->!(A|!A)->(!(A|!A)->!A->!(A|!A)))"
    , "(!A->(A|!A))->((!(A|!A)->!A->!(A|!A))->!(A|!A)->(!(A|!A)->!A->!(A|!A)))"
    , "((!(A|!A)->!A->!(A|!A)))->(!A->(A|!A))->((!(A|!A)->!A->!(A|!A)))"
    , "((!(A|!A)->!A->!(A|!A)))"
    , "(!A->(A|!A))->((!(A|!A)->!A->!(A|!A)))"
    , "((!A->(A|!A))->(!(A|!A)->(!A->!(A|!A))))->((!A->(A|!A))->(!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!(A|!A)->(!A->!(A|!A)))))->((!A->(A|!A))->(!(A|!A)->(!(A|!A)->(!A->!(A|!A)))))"
    , "(((!A->(A|!A))->((!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!(A|!A)->(!A->!(A|!A))))))->((!A->(A|!A))->(!(A|!A)->(!(A|!A)->(!A->!(A|!A))))))"
    , "(!A->(A|!A))->(!(A|!A)->(!(A|!A)->!A->!(A|!A)))"
    , "(!(A|!A)->!(A|!A)->!(A|!A))->(!A->(A|!A))->(!(A|!A)->!(A|!A)->!(A|!A))"
    , "(!(A|!A)->!(A|!A)->!(A|!A))"
    , "(!A->(A|!A))->(!(A|!A)->!(A|!A)->!(A|!A))"
    , "((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))->(!A->(A|!A))->((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "(!A->(A|!A))->((!(A|!A)->!(A|!A)->!(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "((!A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))))->((!A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A)))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A))))->((!A->(A|!A))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A))))"
    , "(((!A->(A|!A))->((!(A|!A)->(!(A|!A)->!(A|!A)))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A)))))->((!A->(A|!A))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A)))))"
    , "(!A->(A|!A))->((!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!(A|!A)->!(A|!A)))"
    , "(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))->(!A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))"
    , "(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))"
    , "(!A->(A|!A))->(!(A|!A)->(!(A|!A)->!(A|!A))->!(A|!A))"
    , "((!A->(A|!A))->(!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A))))->((!A->(A|!A))->(!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A)))->((!A->(A|!A))->(!(A|!A)->!(A|!A)))"
    , "(((!A->(A|!A))->((!(A|!A)->((!(A|!A)->!(A|!A))->!(A|!A)))->(!(A|!A)->!(A|!A))))->((!A->(A|!A))->(!(A|!A)->!(A|!A))))"
    , "(!A->(A|!A))->(!(A|!A)->!(A|!A))"
    , "((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))))->(!A->(A|!A))->((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))))"
    , "((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))))"
    , "(!A->(A|!A))->((!(A|!A)->!(A|!A))->(!(A|!A)->!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))))"
    , "((!A->(A|!A))->(!(A|!A)->!(A|!A)))->((!A->(A|!A))->(!(A|!A)->!(A|!A))->((!(A|!A)->(!(A|!A)->(!A->!(A|!A))))->(!(A|!A)->(!A->!(A|!A)))))->((!A->(A|!A))->((!(A|!A)->(!(A|!A)->(!A->!(A|!A))))->(!(A|!A)->(!A->!(A|!A)))))"
    , "(((!A->(A|!A))->((!(A|!A)->!(A|!A))->((!(A|!A)->(!(A|!A)->(!A->!(A|!A))))->(!(A|!A)->(!A->!(A|!A))))))->((!A->(A|!A))->((!(A|!A)->(!(A|!A)->(!A->!(A|!A))))->(!(A|!A)->(!A->!(A|!A))))))"
    , "(!A->(A|!A))->(((!(A|!A)->(!(A|!A)->(!A->!(A|!A))))->(!(A|!A)->(!A->!(A|!A)))))"
    , "(!(A|!A)->(!A->!(A|!A)))->(!A->(A|!A))->(!(A|!A)->(!A->!(A|!A)))"
    , "(!(A|!A)->(!A->!(A|!A)))"
    , "(!A->(A|!A))->(!(A|!A)->(!A->!(A|!A)))"
    , "((!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))->!!A)->(!(A|!A)->!!A))->(!A->(A|!A))->((!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))->!!A)->(!(A|!A)->!!A))"
    , "((!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))->!!A)->(!(A|!A)->!!A))"
    , "(!A->(A|!A))->((!(A|!A)->(!A->!(A|!A)))->(!(A|!A)->(!A->!(A|!A))->!!A)->(!(A|!A)->!!A))"
    , "((!A->(A|!A))->(!(A|!A)->(!A->!(A|!A))))->((!A->(A|!A))->(!(A|!A)->(!A->!(A|!A)))->((!(A|!A)->((!A->!(A|!A))->!!A))->(!(A|!A)->!!A)))->((!A->(A|!A))->((!(A|!A)->((!A->!(A|!A))->!!A))->(!(A|!A)->!!A)))"
    , "(((!A->(A|!A))->((!(A|!A)->(!A->!(A|!A)))->((!(A|!A)->((!A->!(A|!A))->!!A))->(!(A|!A)->!!A))))->((!A->(A|!A))->((!(A|!A)->((!A->!(A|!A))->!!A))->(!(A|!A)->!!A))))"
    , "(!A->(A|!A))->(((!(A|!A)->((!A->!(A|!A))->!!A))->(!(A|!A)->!!A)))"
    , "((!A->(A|!A))->(!(A|!A)->((!A->!(A|!A))->!!A)))->((!A->(A|!A))->(!(A|!A)->((!A->!(A|!A))->!!A))->(!(A|!A)->!!A))->((!A->(A|!A))->(!(A|!A)->!!A))"
    , "(((!A->(A|!A))->((!(A|!A)->((!A->!(A|!A))->!!A))->(!(A|!A)->!!A)))->((!A->(A|!A))->(!(A|!A)->!!A)))"
    , "(!A->(A|!A))->(!(A|!A)->(!!A))"
    , "!(A|!A)->!!A"
    , "!(A|!A)->!A"
    , "!(A|!A)->!!A"
    , "(!(A|!A)->!A)->(!(A|!A)->!!A)->!!(A|!A)"
    , "(!(A|!A)->!!A)->!!(A|!A)"
    , "!!(A|!A)"
    , "!!(A|!A)->(A|!A)"
    , "A|!A"
    ]

removalEnding = Proof (map parseStmt ["P->A", "!P->A", "P|!P", "A"]) (Var "A") $
        map parseStmt
        [ "(P -> A) -> (!P -> A) -> (P | !P) -> A"
        , "(!P -> A) -> (P | !P) -> A"
        , "(P | !P) -> A"
        , "A"
        ]
