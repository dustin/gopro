module Spec where

import           Control.Lens
import qualified Data.Aeson            as J
import qualified Data.ByteString.Lazy  as BL
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (fromMaybe)
import           Data.These

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           GoPro.Commands.Backup (extractMedia, extractOrig)
import           GoPro.Commands.Sync   (metadataSources)
import           GoPro.File
import           GoPro.Plus.Media      (FileInfo)

import qualified DBSpec

unit_extractMedia :: Assertion
unit_extractMedia = do
  Just fi <- J.decode <$> BL.readFile "test/mediaex.json" :: IO (Maybe FileInfo)
  assertEqual (show fi) [("derivatives/xxx/xxx-var-timelapse_video.mp4","hhttp://d/","http://d/"),
                         ("derivatives/xxx/xxx-var-high_res_proxy_mp4.mp4","hhttp://e/","http://e/"),
                         ("derivatives/xxx/xxx-var-mp4_low.mp4","hhttp://f/","http://f/"),
                         ("derivatives/xxx/xxx-sidecar-ziplabel.zip","hhttp://b/","http://b/"),
                         ("derivatives/xxx/xxx-files-1.JPG", "hhttp://a/", "http://a/"),
                         ("derivatives/xxx/xxx-files-2.JPG", "hhttp://aprime/", "http://aprime/")
                         ] $
    extractMedia "xxx" fi

unit_extractOrig :: Assertion
unit_extractOrig = do
  Just fi <- J.decode <$> BL.readFile "test/mediaex.json" :: IO (Maybe FileInfo)
  assertEqual (show fi) [("derivatives/xxx/xxx-files-1.JPG", "hhttp://a/", "http://a/"),
                         ("derivatives/xxx/xxx-files-2.JPG", "hhttp://aprime/", "http://aprime/")
                         ] $
    extractOrig "xxx" fi

  Just fiv <- J.decode <$> BL.readFile "test/mediavid.json" :: IO (Maybe FileInfo)
  assertEqual (show fi) [("derivatives/xxx/xxx-var-source.mp4", "hhttp://k/", "http://k/")
                         ] $
    extractOrig "xxx" fiv

unit_extractMediaConcat :: Assertion
unit_extractMediaConcat = do
  efi <- J.eitherDecode <$> BL.readFile "test/concat.json" :: IO (Either String FileInfo)
  assertEqual (show efi) (Right [("derivatives/xxx/xxx-var-source-1.mp4","hhttp://AAL","http://AAL"),
                                 ("derivatives/xxx/xxx-var-source-2.mp4","hhttp://AAM","http://AAM"),
                                 ("derivatives/xxx/xxx-var-source-3.mp4","hhttp://AAN","http://AAN"),
                                 ("derivatives/xxx/xxx-var-high_res_proxy_mp4.mp4","hhttp://AAO","http://AAO"),
                                 ("derivatives/xxx/xxx-var-mp4_low.mp4","hhttp://AAP","http://AAP"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.1.mp4","hhttp://AAB","http://AAB"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.antishake.json","hhttp://AAC","http://AAC"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.antishake_horizon.json","hhttp://AAD","http://AAD"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.antishake_horizon_worldlock.json","hhttp://AAE","http://AAE"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.antishake_worldlock.json","hhttp://AAF","http://AAF"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.proxy_antishake.json","hhttp://AAG","http://AAG"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.proxy_horizon_worldlock.json","hhttp://AAH","http://AAH"),
                                 ("derivatives/xxx/xxx-sidecar-gpmf.proxy_worldlock.json","hhttp://AAI","http://AAI"),
                                 ("derivatives/xxx/xxx-files-1.MP4","hhttp://AAA","http://AAA")]
    ) $
    extractMedia "xxx" <$> efi

unit_extractOrigConcat :: Assertion
unit_extractOrigConcat = do
  eciv <- J.eitherDecode <$> BL.readFile "test/concat.json" :: IO (Either String FileInfo)
  assertEqual (show eciv) (Right [("derivatives/xxx/xxx-var-concat.mp4", "hhttp://AAK", "http://AAK")
                                 ]) $
    extractOrig "xxx" <$> eciv

unit_extractMediaDedup :: Assertion
unit_extractMediaDedup = do
  e <- J.eitherDecode <$> BL.readFile "test/trailing.json" :: IO (Either String FileInfo)
  assertEqual (show e) (Right [("derivatives/xxx/xxx-var-source.jpg", "hhttp://a","http://a")]
                       ) $ extractMedia "xxx" <$> e

unit_metadataSources :: Assertion
unit_metadataSources = do
  e <- J.eitherDecode <$> BL.readFile "test/mediaex.json" :: IO (Either String FileInfo)
  assertEqual (show e) (Right [("http://f/","low"),("http://e/","high")]) $ metadataSources <$> e

unit_gpmfGuesses :: Assertion
unit_gpmfGuesses = do
  e <- J.eitherDecode <$> BL.readFile "test/gpmf.json" :: IO (Either String FileInfo)
  assertEqual (show e) (Right [("https://P","low"),
                               ("https://O","high"),
                               ("https://K","concat"),
                               ("https://L","src"),
                               ("https://M","src"),
                               ("https://N","src")]) $ metadataSources <$> e

unit_fileParseGroup :: Assertion
unit_fileParseGroup = do
  let fns = NE.fromList [
        "/some/path/GL010644.LRV",
        "/some/path/GL020648.LRV",
        "/some/path/GL020649.LRV",
        "/some/path/GOPR0645.GPR",
        "/some/path/GOPR0645.JPG",
        "/some/path/GOPR0650.JPG",
        "/some/path/GX010644.MP4",
        "/some/path/GX010644.THM",
        "/some/path/GX010646.MP4",
        "/some/path/GX010647.MP4",
        "/some/path/GX010648.MP4",
        "/some/path/GX010649.MP4",
        "/some/path/GX020648.MP4",
        "/some/path/GX020648.THM",
        "/some/path/GX020649.MP4",
        "/some/path/GX020649.THM",
        "/some/path/Get_started_with_GoPro.url",
        "/some/path/leinfo.sav"]
      grouped = fmap NE.toList <$> parseAndGroup fns
  assertEqual (show grouped) (
    These
      ["/some/path/GL010644.LRV",
       "/some/path/GL020648.LRV",
       "/some/path/GL020649.LRV",
       "/some/path/GOPR0645.GPR",
       "/some/path/GX010644.THM",
       "/some/path/GX020648.THM",
       "/some/path/GX020649.THM",
       "/some/path/Get_started_with_GoPro.url",
       "/some/path/leinfo.sav"]
      [[File {_gpFilePath = "/some/path/GOPR0645.JPG", _gpCodec = GoProJPG, _gpGrouping = NoGrouping 645 "OPR"}],
       [File {_gpFilePath = "/some/path/GOPR0650.JPG", _gpCodec = GoProJPG, _gpGrouping = NoGrouping 650 "OPR"}],
       [File {_gpFilePath = "/some/path/GX010644.MP4", _gpCodec = GoProHEVC, _gpGrouping = BasicGrouping 1 644}],
       [File {_gpFilePath = "/some/path/GX010646.MP4", _gpCodec = GoProHEVC, _gpGrouping = BasicGrouping 1 646}],
       [File {_gpFilePath = "/some/path/GX010647.MP4", _gpCodec = GoProHEVC, _gpGrouping = BasicGrouping 1 647}],
       [File {_gpFilePath = "/some/path/GX010648.MP4", _gpCodec = GoProHEVC, _gpGrouping = BasicGrouping 1 648},
        File {_gpFilePath = "/some/path/GX020648.MP4", _gpCodec = GoProHEVC, _gpGrouping = BasicGrouping 2 648}],
       [File {_gpFilePath = "/some/path/GX010649.MP4", _gpCodec = GoProHEVC, _gpGrouping = BasicGrouping 1 649},
        File {_gpFilePath = "/some/path/GX020649.MP4", _gpCodec = GoProHEVC, _gpGrouping = BasicGrouping 2 649}]]
    )
    grouped

unit_fileParseGroupLoop :: Assertion
unit_fileParseGroupLoop = do
  let fns = NE.fromList [
        "leinfo.sav",
        "Get_started_with_GoPro.url",
        "GXAA0658.MP4",
        "GXAA0659.MP4",
        "GXAA0660.MP4",
        "GXAA0661.MP4",
        "GXAA0662.MP4",
        "GXAA0663.MP4",
        "GLAA0658.LRV",
        "GXAA0663.THM"
        ]
      grouped = fmap NE.toList <$> parseAndGroup fns
  assertEqual (show grouped) (
    These
      ["leinfo.sav","Get_started_with_GoPro.url","GLAA0658.LRV","GXAA0663.THM"]
      [[File {_gpFilePath = "GXAA0658.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 658 "AA"},
        File {_gpFilePath = "GXAA0659.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 659 "AA"},
        File {_gpFilePath = "GXAA0660.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 660 "AA"},
        File {_gpFilePath = "GXAA0661.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 661 "AA"},
        File {_gpFilePath = "GXAA0662.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 662 "AA"},
        File {_gpFilePath = "GXAA0663.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 663 "AA"}]]
    )
    grouped

unit_fileParseGroupLoopTwoDir :: Assertion
unit_fileParseGroupLoopTwoDir = do
  let fns = NE.fromList [
        "a/leinfo.sav",
        "a/Get_started_with_GoPro.url",
        "a/GXAA0658.MP4",
        "a/GXAA0659.MP4",
        "a/GXAA0660.MP4",
        "a/GXAA0661.MP4",
        "a/GXAA0662.MP4",
        "a/GXAA0663.MP4",
        "a/GLAA0658.LRV",
        "a/GXAA0663.THM",
        "b/GXAA0664.MP4"
        ]
      grouped = fmap NE.toList <$> parseAndGroup fns
  assertEqual (show grouped) (
    These
      ["a/leinfo.sav","a/Get_started_with_GoPro.url","a/GLAA0658.LRV","a/GXAA0663.THM"]
      [[File {_gpFilePath = "a/GXAA0658.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 658 "AA"},
        File {_gpFilePath = "a/GXAA0659.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 659 "AA"},
        File {_gpFilePath = "a/GXAA0660.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 660 "AA"},
        File {_gpFilePath = "a/GXAA0661.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 661 "AA"},
        File {_gpFilePath = "a/GXAA0662.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 662 "AA"},
        File {_gpFilePath = "a/GXAA0663.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 663 "AA"}],
        [File {_gpFilePath = "b/GXAA0664.MP4", _gpCodec = GoProHEVC, _gpGrouping = LoopGrouping 664 "AA"}]]
    )
    grouped

unit_fileParseGroupSession5 :: Assertion
unit_fileParseGroupSession5 = do
  let fns = NE.fromList [
        "GOPR2016.LRV",
        "GOPR2016.MP4",
        "GOPR2016.THM",
        "GP012016.LRV",
        "GP012016.MP4"
        ]
      grouped = fmap NE.toList <$> parseAndGroup fns
  assertEqual (show grouped) (
    These
      ["GOPR2016.LRV", "GOPR2016.THM", "GP012016.LRV"]
      [[File {_gpFilePath = "GOPR2016.MP4", _gpCodec = GoProAVC, _gpGrouping = BasicGrouping 1 2016},
        File {_gpFilePath = "GP012016.MP4", _gpCodec = GoProAVC, _gpGrouping = BasicGrouping 1 2016}]]
    )
    grouped

unit_fileParseGroupNonstdJPG :: Assertion
unit_fileParseGroupNonstdJPG = do
  let fns = NE.fromList [
        "GOPR2016.LRV",
        "file1.jpg",
        "file2.jpg",
        "IMG_8008.JPG"
        ]
      grouped = fmap NE.toList <$> parseAndGroup fns
  assertEqual (show grouped) (
    These
      ["GOPR2016.LRV"]
      [[File {_gpFilePath = "file1.jpg", _gpCodec = GoProJPG, _gpGrouping = NoGrouping 0 ""}],
       [File {_gpFilePath = "file2.jpg", _gpCodec = GoProJPG, _gpGrouping = NoGrouping 0 ""}],
       [File {_gpFilePath = "IMG_8008.JPG", _gpCodec = GoProJPG, _gpGrouping = NoGrouping 0 ""}]]
    )
    grouped

unit_nextFile :: Assertion
unit_nextFile = mapM_ (uncurry testOne) [
  ("/blah/GH012345.MP4", "/blah/GH022345.MP4"),
  ("/blah/GOPR1234.MP4", "/blah/GH021234.MP4"),
  ("/blah/GHAA2345.MP4", "/blah/GHAA2346.MP4")
  ]

  where
    testOne input want = do
      case nextFile <$> parseGPFileName input of
        Just got -> assertEqual ("nexting " <> show got) want (_gpFilePath got)
        Nothing  -> fail ("Failed to parse " <> input)
