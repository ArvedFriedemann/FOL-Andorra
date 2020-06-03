module NaiveApproach where

data Term v = UNAS | ADDR v | APPL v v


--return Nothing if no deduction can be made (constraint needs to stay active)

implication :: [Term v] -> Term v -> Term v -> Maybe [Term v]
implication state prem post
  | prem `elem` state = Just [post]
  | (neg_constr prem) `elem` state = Just []
  | otherwise = Nothing

conjunction :: Term v -> Term v -> [Term v]
conjunction a b = [a,b]

disjunction :: [Term v] -> Term v -> Term v -> [Term v]
disjunction state a b
  | (neg_constr a) `elem` state = Just [b]
  | (neg_constr b) `elem` state = Just [a]
  | (a `elem` state) || (b `elem` state) = Just []
  | otherwise = Nothing

--TODO: binders still need local variables!
--Always push forall binders inside as far as possible, or at least their exchange term. If these exchange terms vanish, no exchange needs to be made.
--TODO: Ignore binding for now. Universal variable can just not be read out, existential ones are put into the formula already anywaz. Do normal computation. Additionally, create a copy for every variable and then do normal computation. Wait...that is what I just did. But yeah...general idea, only resolve one quantifier and then do computation before doing anything else. And also do the computation within the quantifiers! important! that preshrinks them.

--Existential quantifier can be extended once, universal one arbitrary many times?
--Universal quantifier -> When just extended, the deductions in the extensions also have to hold for the original. even though they somehow do, don't they?
--If the assignment is not known, other equalities might. Assume an implication is derived, A -> B, then in order to do anything, some A = C has to hold for some fact C. This would already set the equalities. For single facts (or conjunction/disjunction), the equality would go into the single parts. The example of implication could look like "C, A = C -> B". If the C is bound existentially, the matching is done automatically. in the extracted formulae, the former forall bound variables are now instantiated existential ones (so...just new variables).
--Still open question: how many extractions to make? Easily done when conjunction and terms have fixed size ...try everything out.
--assume only one extraction and then propagate. If the rule only applies once somewhere it should just be propagated. If it applied several times, application kinda stops in the middle...
--PS: All of this only works when we just wanna know the existence of a certain truth! In the universal setting, everything would need to be deduced...

neg_constr :: Term v -> Term v
bot :: Term v
top :: Term v
