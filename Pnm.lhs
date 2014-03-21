\documentclass[12pt,a4paper]{article}

\usepackage{fullpage}
\usepackage{minted}
\newminted[code]{haskell}{fontsize=\small}

\title{The \texttt{Pnm} Module}
\author{Roma Klapaukh, Timothy Jones}
\date{11 March 2014}

\begin{document}
\maketitle

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}

\noindent The \texttt{Pnm} module is used to turn an array into a PNM image. It
supports exporting in black and white, greyscale, and colour. The module
exports three functions, one for each of the rendering modes, and the related
data types.

\begin{code}
module Pnm
    ( BW (Black, White)
    , writePnmBW

    , Shade
    , writePnmGrey

    , Rgb (Rgb)
    , writePnmColour
    ) where
\end{code}

\noindent We'll need these imports later.

\begin{code}
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.DeepSeq (NFData)
import Data.Char       (intToDigit)
import Data.List       (intersperse)
import Data.Monoid
import Data.Text       (Text)
import Data.Word       (Word8)
import System.IO       (IOMode (WriteMode), hClose, openFile)
\end{code}

\noindent In order to support the three rendering modes, but avoid duplicating
code, the generic PNM writer function will need to take either black and white,
greyscale shade, or RGB colour pixels. The \texttt{PixelList} data type provides
the union type for these three options, ensuring that the mode is consistent
within a list of pixels.

\begin{code}
data PixelList = BW [BW] | Shade [Shade] | Colour [Rgb]
\end{code}

\noindent Also, because we're working with images, it makes sense to include a
synonym for rectangle dimensions.

\begin{code}
type Dimension = (Int, Int)
\end{code}


\section*{Black and White}

First we define a data type for representing black and white pixels. This is
equivalent to the definition for booleans, but is declared as its own unique
data type to avoid confusion when mapping between the two. Ignore the instance,
that's just for optimising with parallelism.

\begin{code}
data BW = Black | White
    deriving (Eq, Show)

instance NFData BW where
\end{code}

\noindent The first rendering function writes a black and white PNM file. It
accepts a list of \texttt{BW} pixels, a dimension specifying the width and
height of the image, and a \texttt{FilePath} (which is just a synonym for
\texttt{String}). The function simply wraps the pixels into the
\texttt{PixelList} type and passes it to the writer function.

\begin{code}
writePnmBW :: [BW] -> Dimension -> FilePath -> IO ()
writePnmBW = flip writePnm 1 . BW
\end{code}


\section*{Greyscale}

This time, rather than defining our own data type, we need to represent a shade
from 0 to 255. We can do this in a byte, which Haskell refers to as
\texttt{Word8} (with the import from earlier). Integer literals coerce to
\texttt{Word8}, so you can write ordinary numbers and the types will still match
up.

\begin{code}
type Shade = Word8
\end{code}

\noindent The PNM format also allows the definition of a \emph{threshold}, which
indicates the maximum shade value in the file, and then treats that number as
white and scales the actual shade values to match. As the threshold is a shade
value, the type synonym here is easy.

\begin{code}
type Threshold = Shade
\end{code}

\noindent Now we can define the rendering function for greyscale. As with
\texttt{writePnmBW}, it converts the shades to colours and then defers to the
generic renderer.

\begin{code}
writePnmGrey :: [Shade] -> Threshold -> Dimension -> FilePath -> IO ()
writePnmGrey = writePnm . Shade
\end{code}


\section*{Colour}

Finally, we need a definition for colour images. An RGB colour is just a
grouping of three shades.

\begin{code}
data Rgb = Rgb !Shade !Shade !Shade

instance NFData Rgb where
\end{code}

\noindent This allows us to define \texttt{writePnmColour}, which again just
converts and defers.

\begin{code}
writePnmColour :: [Rgb] -> Threshold ->  Dimension -> FilePath -> IO ()
writePnmColour = writePnm . Colour
\end{code}


\section*{Implementation}

Now we get to the meat of the module. PNM can be written in a number of formats.
Each of our modes uses a different format header, so we have a little helper to
convert a \texttt{PixelList} into a header, ignoring the actual list of pixels.

\begin{code}
toHeader :: PixelList -> Text
toHeader (BW _)     = "P1"
toHeader (Shade _)  = "P2"
toHeader (Colour _) = "P3"
\end{code}

\noindent We have a lot of shade values floating around, so let's define a
mechanism for converting them to \texttt{Text} so they can be written to the PNM
file. This is achieved by using \texttt{divMod} to take apart each of the three
possible digits, and writing that digit into \texttt{Text}. Leading zeroes are
ignored.

\begin{code}
toText :: Shade -> Text
toText x = let (d, m) = divMod x 100 in d `cons`
    let (d', m') = divMod m 10 in d' `cons` sing m'
  where
    toChar = intToDigit . fromIntegral
    cons = cons' . toChar
    cons' '0' = id
    cons' c   = T.cons c
    sing = T.singleton . toChar
\end{code}

\noindent The last of our helpers is a function to convert a list of pixels into
\texttt{[Text]}, so that pixels can be written to the file one at a time. This
is important, as it allows the writing to occur in constant memory.

\begin{code}
render :: PixelList -> [Text]
render (BW pixels)     = map bnw pixels
  where
    bnw Black = "0"
    bnw White = "1"
render (Shade pixels)  = map toText pixels
render (Colour pixels) = map rgb pixels
  where
    rgb (Rgb r g b) = toText r <> " " <> toText g <> " " <> toText b
\end{code}

\noindent The generic \texttt{writePnm} accepts a \texttt{PixelList}, along with
the threshold, size, and file path.

\begin{code}
writePnm :: PixelList -> Threshold -> Dimension -> FilePath -> IO ()
writePnm pixels threshold (w, h) fp = do
\end{code}

\noindent We're going to open and close the file manually, because lazy IO is
the devil but we still want to be able to stream a pixel list into the file. To
aid us in this, we define a local function \texttt{write} that writes a chunk of
\texttt{Text} to the file.

\begin{code}
    file <- openFile fp WriteMode
    let write = T.hPutStr file
\end{code}

\noindent First we write the header and image size, and then the threshold if
we're not in black and white mode. The \texttt{return ()} \emph{doesn't} cause
the function to exit, because that's not what \texttt{return} does in Haskell
(it's not even a keyword, we're just applying a function named \texttt{return}).

\begin{code}
    write $ toHeader pixels <> "\n" <>
      (T.pack . show) w <> " " <> (T.pack . show) h <> "\n"
    case pixels of
        BW _ -> return ()
        _    -> write $ toText threshold <> "\n"
\end{code}

\noindent Then we write each of the pixels in turn, with spaces in between, and
close the file.

\begin{code}
    mapM_ write $ intersperse " " (render pixels)
    hClose file
\end{code}

\end{document}

