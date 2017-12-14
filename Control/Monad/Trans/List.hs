{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Control.Monad.Trans.List (ListT (..), fromList, toListM, toReverseListM, foldlM) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty (..), intersperse)
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup

newtype ListT m a = ListT { runListT :: m (Maybe (a, ListT m a)) }
  deriving (Functor, Foldable, Traversable)

fromList :: Monad m => [a] -> ListT m a
fromList = ListT . pure . \ case [] -> Nothing
                                 x:xs -> Just (x, fromList xs)

toListM :: Monad m => ListT m a -> m [a]
toListM = fmap ($ []) . foldlM (\ f a -> pure (f . (a:))) id

toReverseListM :: Monad m => ListT m a -> m [a]
toReverseListM = foldlM (\ as a -> pure (a:as)) []

foldlM :: Monad m => (b -> a -> m b) -> b -> ListT m a -> m b
foldlM f b = runListT >=> maybe (pure b) (\ (a, as) -> f b a >>= flip (foldlM f) as)

instance MonadTrans ListT where lift = ListT . fmap (Just . flip (,) empty)

instance Eq1 m => Eq1 (ListT m) where
    liftEq (==) (ListT x) (ListT y) =
        (liftEq . liftEq) (\ (x, xs) (y, ys) -> x == y && liftEq (==) xs ys) x y

instance Ord1 m => Ord1 (ListT m) where
    liftCompare cmp (ListT x) (ListT y) =
        (liftCompare . liftCompare) (\ (x, xs) (y, ys) ->
                                     x `cmp` y <> liftCompare cmp xs ys) x y

instance Show1 m => Show1 (ListT m) where
    liftShowsPrec sp sl n (ListT x) = fst (show1Methods sp sl) n x
    liftShowList sp sl = snd (show1Methods sp sl) . fmap runListT

show1Methods sp sl =
    (l . l) (pure f,
             list id $
             between '[' ']' . appEndo . intercalate (Endo (", " ++)) . fmap (Endo . f))
  where l :: Show1 f => (Int -> a -> ShowS, [a] -> ShowS) -> (Int -> f a -> ShowS, [f a] -> ShowS)
        l (sp, sl) = (liftShowsPrec sp sl, liftShowList sp sl)

        between :: a -> a -> ([a] -> [a]) -> [a] -> [a]
        between x y f = (:) x . f . (:) y

        f (x, xs) = between '(' ')' $ sp 0 x . (++) ", " . liftShowsPrec sp sl 0 xs

instance (Eq a, Eq1 m) => Eq (ListT m a) where (==) = liftEq (==)
instance (Ord a, Ord1 m) => Ord (ListT m a) where compare = liftCompare compare
instance (Show a, Show1 m) => Show (ListT m a) where
    showsPrec = liftShowsPrec showsPrec showList
    showList = liftShowList showsPrec showList

instance Applicative p => Applicative (ListT p) where
    pure x = ListT . pure $ Just (x, ListT (pure Nothing))
    ListT xm <*> ListT ym = ListT ((liftA2 . liftA2) go xm ym)
      where go (x, xs) (y, ys) = (x y, x <$> ys <|> xs <*> ListT ym)

instance Applicative p => Alternative (ListT p) where
    empty = (ListT . pure) Nothing
    ListT xm <|> ys@(ListT ym) = ListT (liftA2 go xm ym)
      where go = \ case Nothing -> id
                        Just (x, xs) -> pure $ Just (x, xs <|> ys)

instance Monad m => Monad (ListT m) where
    xm >>= f = join (f <$> xm)
      where join (ListT xm) = ListT $ xm >>= \ case
                Nothing -> pure Nothing
                Just (ListT ym, xss) -> ym >>= \ case
                    Nothing -> runListT (join xss)
                    Just (y, ys) -> (pure . Just) (y, ys <|> join xss)

instance Monad m => MonadPlus (ListT m) where
    ListT xm `mplus` ys = ListT $ xm >>= \ case
        Nothing -> runListT ys
        Just (x, xs) -> (pure . Just) (x, xs `mplus` ys)

instance MonadFix m => MonadFix (ListT m) where
    mfix f = ListT $ (flip fmap . mfix) (runListT . f . fst . fromJust) . fmap $
             id *** (pure . mfix $
                     ListT <<< runListT . f >=> \ case Just (_, xs) -> runListT xs
                                                       Nothing -> error "Nothing")

intercalate :: Semigroup a => a -> NonEmpty a -> a
intercalate a = sconcat . intersperse a

list :: b -> (NonEmpty a -> b) -> [a] -> b
list x _ [] = x
list _ f (x:xs) = f (x:|xs)
