buildTower :: Int -> [String]
buildTower size
   | size == 1 = ["*"]
   | size == 2 = ["*","***"] 