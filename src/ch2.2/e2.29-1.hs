
import Control.Monad


type Weight = Int

data Structure w m = w | m

type Branch = (Int, Structure)

type Mobile = (Branch, Branch)


makeMobile l r = (l, r)

makeBranch l s = (l, s)


leftBranch :: Mobile -> Branch
leftBranch (l,_) = l

rightBranch :: Mobile -> Branch
rightBranch (_,r) = r


weightBranch :: Branch -> Int
weightBranch (_, (Weight w)) = w
weightBranch (_, (Mobile m)) = totalWeight m


lengthBranch :: Branch -> Int
lengthBranch (l, _) = l


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
torqueBranch b = (lengthBranch b) * (weightBranch b)


balancedBranch :: Branch -> Bool
balancedBranch (_, (Left _))  = True
balancedBranch (_, (Right m)) = balanced m


balanced :: Mobile -> Bool
balanced m =    (balancedBranch l)
             && (balancedBranch r)
             && ((torqueBranch l) == (torqueBranch r))
  where l = leftBranch m
        r = rightBranch m


balancedStructure :: Structure -> Bool
balancedStructure (Left w) = True
balancedStructure (Right m) = balanced m


main = do
  print $ totalWeight mobile
  print $ balanced mobile
  print $ balanced mobileBalanced
  
