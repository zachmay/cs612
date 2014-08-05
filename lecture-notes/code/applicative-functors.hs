maybeApplyToMaybe :: Maybe (Integer -> Integer)
                  -> Maybe Integer
                  -> Maybe Integer
maybeApplyToMaybe (Just f) (Just x) = Just $ f x
maybeApplyToMaybe _ _ = Nothing

class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure x            = Just x
    Just f <*> Just x = Just $ f x
    _ <*> _           = Nothing

(<$>) :: (a -> b) -> f a -> f b
f <$> v = pure f <*> v

createPlaneTicket
    :: Maybe Section
    -> Maybe MealOption
    -> Maybe PlaneTicket
createPlaneTicket section meal =
    PlaneTicket <$> section <*> meal

instance Applicative [] where
    pure x    = [x]
    gs <*> xs = [ g x | g <- gs, x <- xs ]

newtype ZipList a = ZipList { getZipList :: [a] }
 
instance Functor ZipList where
    fmap f (ZipList xs) =
        ZipList $ map f xs

instance Applicative ZipList where
    pure x = ZipList $ repeat x
    (ZipList gs) <*> (ZipList xs) =
        ZipList $ zipWith ($) gs xs



data PlaneTicket
    = PlaneTicket Section MealOption

data Section = Coach
             | Business
             | FirstClass

data MealOption = Regular
                | Vegetarian

data TravelDetails = Train
                   | Automobile
                   | Plane PlaneTicket

