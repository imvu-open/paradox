{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Makes the standard font relocatable.
module Paradox.Render.LocalFonts (localFonts) where

import Graphics.Rendering.Chart.Backend.Types

import System.FilePath                              (replaceFileName)
import Graphics.Rendering.Chart.Backend.Diagrams    (
                                                    FontSelector
                                                    )

import qualified Graphics.SVGFonts                  as F
import qualified Graphics.SVGFonts.ReadFont         as F

type DFont n = F.PreparedFont n


localFonts :: forall n. (RealFloat n, Read n)
           => FilePath
           -> IO (FontSelector n)
localFonts exec = do
    serifR   <- F.loadFont (replaceFileName exec "fonts/LinLibertine_R.svg")
    serifRB  <- F.loadFont (replaceFileName exec "fonts/LinLibertine_RB.svg")
    serifRBI <- F.loadFont (replaceFileName exec "fonts/LinLibertine_RBI.svg")
    serifRI  <- F.loadFont (replaceFileName exec "fonts/LinLibertine_RI.svg")
    sansR    <- F.loadFont (replaceFileName exec "fonts/SourceSansPro_R.svg")
    sansRB   <- F.loadFont (replaceFileName exec "fonts/SourceSansPro_RB.svg")
    sansRBI  <- F.loadFont (replaceFileName exec "fonts/SourceSansPro_RBI.svg")
    sansRI   <- F.loadFont (replaceFileName exec "fonts/SourceSansPro_RI.svg")
    monoR    <- F.loadFont (replaceFileName exec "fonts/SourceCodePro_R.svg")
    monoRB   <- F.loadFont (replaceFileName exec "fonts/SourceCodePro_RB.svg")

    let selectFont :: FontStyle
                   -> F.PreparedFont n
        selectFont fs = case (_font_name fs, _font_slant fs, _font_weight fs) of
          ("serif", FontSlantNormal , FontWeightNormal) -> alterFontFamily "serif" serifR
          ("serif", FontSlantNormal , FontWeightBold  ) -> alterFontFamily "serif" serifRB
          ("serif", FontSlantItalic , FontWeightNormal) -> alterFontFamily "serif" serifRI
          ("serif", FontSlantOblique, FontWeightNormal) -> alterFontFamily "serif" serifRI
          ("serif", FontSlantItalic , FontWeightBold  ) -> alterFontFamily "serif" serifRBI
          ("serif", FontSlantOblique, FontWeightBold  ) -> alterFontFamily "serif" serifRBI

          ("sans-serif", FontSlantNormal , FontWeightNormal) -> alterFontFamily "sans-serif" sansR
          ("sans-serif", FontSlantNormal , FontWeightBold  ) -> alterFontFamily "sans-serif" sansRB
          ("sans-serif", FontSlantItalic , FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
          ("sans-serif", FontSlantOblique, FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
          ("sans-serif", FontSlantItalic , FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI
          ("sans-serif", FontSlantOblique, FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI

          ("monospace", _, FontWeightNormal) -> alterFontFamily "monospace" monoR
          ("monospace", _, FontWeightBold  ) -> alterFontFamily "monospace" monoRB

          (fam, FontSlantNormal , FontWeightNormal) | fam `isFontFamily` serifR   -> serifR
          (fam, FontSlantNormal , FontWeightBold  ) | fam `isFontFamily` serifRB  -> serifRB
          (fam, FontSlantItalic , FontWeightNormal) | fam `isFontFamily` serifRI  -> serifRI
          (fam, FontSlantOblique, FontWeightNormal) | fam `isFontFamily` serifRI  -> serifRI
          (fam, FontSlantItalic , FontWeightBold  ) | fam `isFontFamily` serifRBI -> serifRBI
          (fam, FontSlantOblique, FontWeightBold  ) | fam `isFontFamily` serifRBI -> serifRBI

          (fam, FontSlantNormal , FontWeightNormal) | fam `isFontFamily` sansR   -> sansR
          (fam, FontSlantNormal , FontWeightBold  ) | fam `isFontFamily` sansRB  -> sansRB
          (fam, FontSlantItalic , FontWeightNormal) | fam `isFontFamily` sansRI  -> sansRI
          (fam, FontSlantOblique, FontWeightNormal) | fam `isFontFamily` sansRI  -> sansRI
          (fam, FontSlantItalic , FontWeightBold  ) | fam `isFontFamily` sansRBI -> sansRBI
          (fam, FontSlantOblique, FontWeightBold  ) | fam `isFontFamily` sansRBI -> sansRBI

          (fam, _, FontWeightNormal) | fam `isFontFamily` monoR  -> monoR
          (fam, _, FontWeightBold  ) | fam `isFontFamily` monoRB -> monoRB

          (_, _, _) -> selectFont (fs { _font_name = "sans-serif" })

    return selectFont


alterFontFamily :: String
                -> DFont n
                -> DFont n
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)

isFontFamily :: String
             -> DFont n
             -> Bool
isFontFamily n (fd, _) = n == F.fontDataFamily fd
