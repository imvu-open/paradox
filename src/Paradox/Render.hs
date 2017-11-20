{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paradox.Render
(
  mkSvgResult
, SvgResult(..)
)
where

import Prelude                                      hiding (concatMap)

import Control.Monad.IO.Class

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import Paradox.Eval                                 ()
import Data.Maybe                                   (fromJust)

import Data.Foldable                                (concatMap)
import Control.Arrow                                ((&&&))

import Data.Time.LocalTime.TimeZone.Series          ( utcToLocalTime'
                                                    , TimeZoneSeries
                                                    )
import Codec.Picture.Saving                         (imageToPng)
import Codec.Picture.Types                          (DynamicImage(..))
import "svg-tree" Graphics.Svg                      (parseSvgFile)
import Graphics.Rasterific.Svg                      ( loadCreateFontCache
                                                    , renderSvgDocument
                                                    )

import qualified Paradox.Istatd.Types               as IT
import qualified Data.Text                          as TS
import qualified Data.ByteString.Lazy.Char8         as BSLC
import qualified Data.Map                           as M
import qualified Graphics.Svg.Core                  as Svg
import qualified Data.Time.Clock.POSIX              as POSIX
import qualified Data.List.NonEmpty                 as NonEmpty
import qualified Diagrams.Backend.SVG               as DSVG
import qualified Diagrams.Prelude                   as D
import qualified Diagrams.TwoD                      as D2


data SvgResult = SvgResult
    { svgValue :: Svg.Element
    , toBytes :: forall m a. Monad m => (BSLC.ByteString -> m a) -> m a
    , toPng :: forall m a. (Monad m, MonadIO m) => (Maybe BSLC.ByteString -> m a) -> m a
    , toPngAndBytes :: forall m a. (Monad m, MonadIO m) => (Maybe BSLC.ByteString -> BSLC.ByteString -> m a) -> m a
    }


myColors :: [AlphaColour Double]
myColors = cycle $ map opaque [ black
                              , blue
                              , brown
                              , chartreuse
                              , chocolate
                              , darkgoldenrod
                              , darkgreen
                              , darkmagenta
                              , honeydew
                              , indianred
                              , indigo
                              , lime
                              , maroon
                              , mediumaquamarine
                              , midnightblue
                              , moccasin
                              , peru
                              , pink
                              , purple
                              , red
                              , springgreen
                              , teal
                              , yellow
                              ]

renderQueryS :: M.Map TS.Text IT.GraphData
             -> IT.ParadoxReply
             -> TimeZoneSeries
             -> Renderable ()
renderQueryS graphs pq tzs = fillBackground (FillStyleSolid (opaque white))
                       $ renderStackedLayouts
                       $ slayouts_layouts .~ layouts
                       $ slayouts_compress_legend .~ False
                       $ def
    where
    toLocalTime = utcToLocalTime' tzs . POSIX.posixSecondsToUTCTime . fromIntegral
    layouts = M.elems $ M.mapWithKey (\k gd ->
        StackedLayout $ layout k gd
            ) graphs

    layout k gd =
        let (elems, min',max') = foldr (\cexpr (l, accmin :: Double, accmax :: Double) ->
                let findIt = IT.ceQS cexpr
                    pr = fromJust $ M.lookup findIt (IT.pprQueries pq)
                    avgs = concatMap (NonEmpty.toList . NonEmpty.map IT.tscAvg)
                                     (M.elems $ M.map IT.ptsData (IT.timeSeriesMap pr))
                    maxi = maximum $ accmax:avgs
                    mini = minimum $ accmin:avgs
                    plots ts = map ((toLocalTime . IT.tscTime) &&& IT.tscAvg)
                                   (NonEmpty.toList $ IT.ptsData ts)
                    plotV (IT.CounterSpec n _) ts =
                        let plotWithColor color = plot_lines_values .~ [plots ts]
                                                $ plot_lines_title .~ TS.unpack n
                                                $ plot_lines_style . line_color .~ color
                                                $ def
                        in plotWithColor
                in (M.elems (M.mapWithKey plotV (IT.timeSeriesMap pr)):l, mini, maxi)
              ) ([], 1/0,0) (IT.gdKeys gd)
            plotHidden = let slop = (max' - min') * 0.05
                             max'' = max' + (slop * 2)
                             min'' = if min' >= 0 && (min' - slop < 0)
                                        then 0
                                        else min' - slop
                         in plot_hidden_y_values .~ [min'', max''] $ PlotHidden [] []
            elems' = concat elems
        in layout_title .~ TS.unpack k
         $ layout_plots .~ toPlot plotHidden:map toPlot (zipWith ($) elems' myColors)
         $ layout_x_axis . laxis_generate .~ autoTimeAxis
         $ def

renderToSVGS :: FontSelector Double
             -> TimeZoneSeries
             -> M.Map TS.Text IT.GraphData
             -> IT.ParadoxReply
             -> Double
             -> Double
             -> IO (Svg.Element , PickFn ())
renderToSVGS fontSelector tzs graphs pq = myRenderableToSVG fontSelector (renderQueryS graphs pq tzs)

mkSvgResult :: FontSelector Double
            -> TimeZoneSeries
            -> M.Map TS.Text IT.GraphData
            -> IT.ParadoxReply
            -> Double
            -> Double
            -> IO SvgResult
mkSvgResult fontSelector tzs graphs pq width height =
    let numGraphs = length $ M.elems graphs
        trueHeight = height * fromIntegral numGraphs
    in do
        (x, bs) <- ( id &&& Svg.renderBS) . fst <$> renderToSVGS fontSelector tzs graphs pq width trueHeight
        return SvgResult
            { svgValue = x
            , toBytes = \act -> act bs
            , toPng = \act -> liftIO (renderSvgToPng x) >>= act
            , toPngAndBytes = \act -> do
                pngValue <- liftIO $ renderSvgToPng x
                let svgBytes = bs
                act pngValue svgBytes
            }

myRenderableToSVG :: FontSelector Double
                  -> Renderable a
                  -> Double
                  -> Double
                  -> IO (Svg.Element , PickFn a)
myRenderableToSVG f r w h = do
    let env = createEnv vectorAlignmentFns w h f
    return $ renderableToSVG' r env

renderSvgToPng :: Svg.Element
               -> IO (Maybe BSLC.ByteString)
renderSvgToPng svg = do
  let bs' = Svg.renderBS svg
      bs = BSLC.toStrict bs'
      parsed = parseSvgFile "" bs
  case parsed of
     Nothing -> return Nothing
     Just doc -> do
        cache <- loadCreateFontCache "/tmp/fonty-texture-cache"
        (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
        let pngData = imageToPng $ ImageRGBA8 finalImage
        return $ Just pngData

-- | Output the given renderable as a SVG using the given environment.
renderableToSVG' :: Renderable a -> DEnv Double -> (Svg.Element, PickFn a)
renderableToSVG' r env =
  let (w, h) = envOutputSize env
      (d, x) = runBackendR env r
      opts = DSVG.SVGOptions (D2.dims2D w h) Nothing TS.empty [] True
      svg = D.renderDia DSVG.SVG opts d
  in (svg, x)
