import Control.Monad

data Mobile = Mobile {
      leftBranch  :: Branch
    , rightBranch :: Branch
    } deriving (Show)


makeMobile :: Branch -> Branch -> Mobile
makeMobile l r = Mobile l r

makeBranch :: Int -> Structure -> Branch
makeBranch l s = Branch l s


type Weight = Int

type Structure = Either Weight Mobile

data Branch = Branch {
      branchLength :: Int
    , branchStructure :: Structure
    } deriving (Show)


weightBranch :: Branch -> Int
weightBranch (Branch l (Left w)) = w
weightBranch (Branch l (Right m)) = totalWeight m


totalWeight :: Mobile -> Int
totalWeight m = (weightBranch (leftBranch m)) + (weightBranch (rightBranch m))


mobile = makeMobile
          ( makeBranch
              10
              ( Left 100))
          ( makeBranch
              20
              ( Right (makeMobile
                        (makeBranch 21 (Left 200))
                        (makeBranch 22 (Left 220)))))

mobileBalanced = makeMobile
          ( makeBranch
              10
              ( Left 100))
          ( makeBranch
              10
              ( Left 100))


torqueBranch :: Branch -> Int
torqueBranch (Branch l (Left w)) = l * w
torqueBranch (Branch l (Right m)) = l * (totalWeight m)

balanced :: Mobile -> Bool
balanced (Mobile l r) = (balancedBranch l)
                      && (balancedBranch r)
                      && ((torqueBranch l) == (torqueBranch r))
  where balancedBranch (Branch l (Left w)) = True
        balancedBranch (Branch l (Right m)) = balanced m


main = do
  print $ totalWeight mobile
  print $ balanced mobile
  print $ balanced mobileBalanced
  
