data OperatingSystem =
  GnuPlusLinux |
  OpenBSD |
  Mac |
  Windows
  deriving (Eq, Show, Enum, Bounded)

data ProgrammingLanguage =
  Haskell |
  Agda |
  Idris |
  PureScript
  deriving (Eq, Show, Enum, Bounded)

data Programmer = Programmer {os :: OperatingSystem,
                              lang :: ProgrammingLanguage}
                  deriving (Eq, Show)

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound ..]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allValues, lang <- allValues]
