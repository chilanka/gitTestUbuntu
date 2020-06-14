-- runghc AnySong.hs
--ffplay -showmode 1 -f f32le -ar 48000 output.bin


module Main where
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf 
import Data.List -- for ZipWith3
import System.IO  

inputFilepath::FilePath 
inputFilepath="input.txt"

outputFilepath::FilePath -- synonym to String only
outputFilepath="output.bin"

type Seconds = Float --alias
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitones = Float
type Beats = Float

bpm :: Beats
bpm = 120.0

 

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 48000

pitchStd :: Hz
pitchStd = 440.0

beatDuration :: Seconds
beatDuration = 60.0/bpm


f :: Semitones -> Hz
f n = pitchStd * (2 ** (1.0/12.0)) ** n

--note :: Semitones -> Seconds -> [Pulse]
note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)


freq:: Hz -> Seconds -> [Pulse]
freq hz duration =  map (* volume) $ zipWith3 (\x y z -> x*y*z) release attack output
      where step= (hz*2*pi)/sampleRate

            output :: [Pulse]
            output=map sin $ map (* step) [0.0.. sampleRate* duration]

            attack :: [Pulse]
            attack = map (min 1.0) [0.00, 0.001 ..]

            release :: [Pulse]
            release = reverse $ take (length output) attack

{-wave :: [Pulse]
wave = concat [note i duration | i <- [3,3,10,10,12,12,10,8,8,7,7,5,5,3]]
     where
         duration=1.0
-}
--noteList1 :: [Pulse]
--noteList1 =[3,3,10,10,12,12,10,8,8,7,7,5,5,3]

wave :: [Pulse]-> [Pulse]
wave noteList = concat [note i duration | i <- noteList ]
     where
         duration=1.0


save :: FilePath -> [Pulse] -> IO ()
save filePath notelist= B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE $ wave notelist

ftxtFlt :: [String] -> [Pulse]
ftxtFlt = map read

{-play :: IO()
play = do
        let notelist1 = []
        handle <- openFile inputFilepath ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        let notelist1 = ftxtFlt singlewords
        save outputFilepath notelist1
        _ <- runCommand $ printf "ffplay -f f32le -ar %f %s" sampleRate outputFilepath 
        return()-}

main :: IO()
--main = play
main = do
        let notelist1 = []
        handle <- openFile inputFilepath ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        let notelist1 = ftxtFlt singlewords
        save outputFilepath notelist1
        return()
      