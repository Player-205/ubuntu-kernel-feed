module Options where




import Version



data Options = Options 
  { minVersion :: Version
  , minDebs    :: Version
  , minChanges :: Version
  , noRC :: Bool
  }