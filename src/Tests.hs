
module Tests (
    and_tests, regression
) where

-- these are tautologies under the assumption that the atomic propostitions are defined
tautologies = ["Forall (Sofar (AP \"o0\" 0))","Neg (Exists (Y (Const True)))", "Forall (Neg (Y (Const False)))", "Forall (X (Neg (Y (Y (Const False)))))"]

and_tests filePrefix = [
    [filePrefix ++ "and.aag", 
     "Forall (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0)))"],
    [filePrefix ++ "and.aag", 
     "Exists (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0)))"],
    [filePrefix ++ "and.aag", 
     "Exists (AP \"i0\" 0)"],
    [filePrefix ++ "and.aag", 
     "Exists (AP \"o0\" 0)"],
    [filePrefix ++ "and.aag", 
     "Neg (Forall (AP \"i0\" 0))"],
    [filePrefix ++ "and.aag", 
     "Exists (Exists (AP \"i0\" 0))"],
    [filePrefix ++ "and.aag", 
     "Exists (X (AP \"i0\" 0))"],
    [filePrefix ++ "and.aag", 
     "Exists (X (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0))))"],
    [filePrefix ++ "and.aag", 
     "Forall (X (X (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0)))))"],
    [filePrefix ++ "and.aag", 
     "Forall (X (Y (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0)))))"],
    [filePrefix ++ "and.aag", 
     "Exists (X (Y (AP \"o0\" 0)))"],
    [filePrefix ++ "and.aag", 
     "Neg (Exists (Y (AP \"o0\" 0)))"],
    [filePrefix ++ "and.aag", 
     "Neg (Exists (Y (Const True)))"],
    [filePrefix ++ "and.aag", 
     "Forall (Sofar (AP \"o0\" 0))"],
    [filePrefix ++ "and.aag", 
     "Forall (G (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0))))"],
    [filePrefix ++ "and.aag", 
     "Forall (Forall (G (Eq (AP \"o0\" 0) (AP \"o0\" 1))))"],
    [filePrefix ++ "and.aag", 
     "Forall (G (Sofar (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0)))))"],
    [filePrefix ++ "and.aag", 
     "Forall (Forall (G (And (Eq (AP \"o0\" 0) (And (AP \"i0\" 0) (AP \"i1\" 0))) (Eq (AP \"o0\" 1) (And (AP \"i0\" 1) (AP \"i1\" 1))))))"]
    ]
    
regression filePrefix = [
    [filePrefix ++ "toggle.aag", 
     "Forall (G (Eq (AP \"o0\" 0) (Neg (X (AP \"o0\" 0)))))"],
    [filePrefix ++ "toggle-re.aag", 
     "Forall (G (Or (AP \"reset\" 0)) (Eq (AP \"Q\" 0) (X (AP \"!Q\" 0))))"],
    [filePrefix ++ "buffer.aag", 
     "Forall (G (Or (AP \"reset\" 0)) (Eq (AP \"Q\" 0) (X (AP \"!Q\" 0))))"]
    ]