module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortBy, groupBy, intercalate)
import Data.List.Split (split, keepDelimsL, whenElt)
import Data.Map.Strict(Map, fromList, (!?))

getLines :: Int -> String -> IO [[String]]
getLines dropcnt filename = map words . drop dropcnt . lines <$> readFile filename

data NetSection = NetSection String [[String]]

data Pin = Pin { pinPartName :: String, pinPadName :: String}
instance Show Pin where
  show (Pin ptn pdn) = ptn ++ "." ++ pdn

data PinAssoc = PinAssoc { pinAssocNetName :: String , pinAssocPin :: Pin }
instance Show PinAssoc where
  show (PinAssoc nn pn) = (show pn) ++ "on" ++ nn

ofPadNames :: (String -> String -> a) -> PinAssoc -> PinAssoc -> a
ofPadNames f (PinAssoc _ (Pin _ a)) (PinAssoc _ (Pin _ b)) = f a b

ofPartNames :: (String -> String -> a) -> PinAssoc -> PinAssoc -> a
ofPartNames f (PinAssoc _ (Pin a _)) (PinAssoc _ (Pin b _)) = f a b

cmpPartName :: PinAssoc -> PinAssoc -> Ordering
cmpPartName = ofPartNames compare

cmpPadName :: PinAssoc -> PinAssoc -> Ordering
cmpPadName = ofPadNames compare

samePartName :: PinAssoc -> PinAssoc -> Bool
samePartName = ofPartNames (==)

splitSections :: [[String]] -> [[[String]]]
splitSections = split (keepDelimsL $ whenElt ((==5) . length))

firstLineToNormalLine :: [String] -> (String, [String])
firstLineToNormalLine (netname:firstLineInfo) = (netname, firstLineInfo)

parseLine :: [String] -> Maybe Pin
parseLine [part, _, namedPin, _] = Just $ Pin part namedPin
parseLine _ = Nothing

data Net = Net String [Pin]
instance Show Net where
  show (Net n pns) = "Net " ++ n ++ " has pins " ++ (show pns)

parseSection :: [[String]] -> Maybe Net
parseSection [] = Nothing
parseSection (firstLine:restOfSection) =
  let (name, firstLinePinInfo) = firstLineToNormalLine firstLine in
    Just $ Net name ((fromJust $ parseLine firstLinePinInfo):(catMaybes $ map parseLine restOfSection))

loadAndParse :: String -> IO [Net]
loadAndParse filename = catMaybes <$> map parseSection <$> splitSections <$> getLines 8 filename

smashNet :: Net -> [PinAssoc]
smashNet (Net netname pins) = map (PinAssoc netname) pins

smashNetList :: [Net] -> [PinAssoc]
smashNetList nets = concat $ map smashNet nets

data DevicePin = DevicePin String String
instance Show DevicePin where
  show (DevicePin pad net) = "\t." ++ pad ++ "\t(" ++ net ++ ")"

data Device = Device { devicePartName :: String, devicePartPins :: [DevicePin] }
instance Show Device where
  show (Device deviceName devicePins) = deviceName ++ "(\n" ++ (intercalate ",\n" $ map show devicePins) ++ ");"

data IdentifiedDevice = IdentifiedDevice String Device
instance Show IdentifiedDevice where
  show (IdentifiedDevice deviceVal deviceInstance) = deviceVal ++ " " ++ show deviceInstance

makeDevicePin :: PinAssoc -> DevicePin
makeDevicePin (PinAssoc net (Pin _ pad)) = DevicePin pad net

makeDevice :: [PinAssoc] -> Device
makeDevice pinAssocs = Device (pinPartName . pinAssocPin $ head pinAssocs) $ map makeDevicePin $ sortBy cmpPadName pinAssocs

groupNetListByDevice :: [Net] -> [Device]
groupNetListByDevice nets = map makeDevice $ ((groupBy samePartName) . (sortBy cmpPartName) . smashNetList) nets

parsePartsListLine :: [String] -> Maybe (String, String)
parsePartsListLine (partName:partValue:_) = Just (partName, partValue)
parsePartsListLine _ = Nothing

loadPartsMap :: String -> IO (Map String String)
loadPartsMap filename = fromList <$> catMaybes <$> map parsePartsListLine <$> getLines 10 filename

identifyPart :: Map String String -> Device -> Maybe IdentifiedDevice
identifyPart partMap device = case partMap !? devicePartName device of
  Just deviceVal -> Just $ IdentifiedDevice deviceVal device
  Nothing -> Nothing

verilogFromFile :: String -> IO (String)
verilogFromFile filenamePrefix = do
  partMap <- loadPartsMap $ filenamePrefix ++ "_parts"
  partDefs <- groupNetListByDevice <$> loadAndParse "testdata/dma"
  return $ intercalate "\n" $ map show $ catMaybes $ map (identifyPart partMap) partDefs

main :: IO ()
main = getArgs >>= parseArgs >>= putStr

parseArgs :: [String] -> IO String
parseArgs [fname] = verilogFromFile fname