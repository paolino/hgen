module Applications.SvgSalesman (toSvg) where

type Color = (Int, Int, Int)
stdColor = [(0,0,0),(150,0,0),(0,150,0),(0,0,150),(150,150,0),(150,0,150),(0,150,150),(150,150,150)]
        
data SVG = SVG {width :: Int, height :: Int , elems :: [SvgElem]} deriving (Show)
data SvgElem = SvgElem {name :: String, attrs :: [(String,String)] , text :: Maybe String}deriving (Show)

mkSvg w h elems = SVG w h elems

mkCircle x y r color = SvgElem "circle" [("cx",show x),("cy",show y),("r",show r),("color","rgb" ++ (show color))] Nothing 
mkLine x1 y1 x2 y2 color = SvgElem "line" [("x1",show x1),("y1",show y1),("x2",show x2),("y2",show y2),("stroke","rgb" ++ (show color))] Nothing 

drawSvg (SVG w h l) = "<?xml version=\"1.0\"?>\n" ++"<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"" ++ (show w)++"\" height=\"" ++ (show h)++"\">\n" ++
        drawElemList l ++ "</svg>"
        
drawElemList (x:[]) = drawElem x 
drawElemList (x:xs) = drawElem x ++ drawElemList xs

drawElem (SvgElem name attrs text) = "<" ++ name ++ (drawAttrs attrs) ++ "/>\n" 

drawAttrs l = foldl drawAttr "" l

drawAttr z (k,v) = z ++ " " ++ k ++ "=\"" ++ v ++ "\""


salesman :: [(Float,Float)] -> Color -> [SvgElem]
salesman l color = salesman_ l [] color

salesman_ ((x1,y1):[]) l color = mkCircle x1 y1 2 color:l
salesman_ ((x1,y1):(x2,y2):xs) l  color = mkCircle x1 y1 2 color : mkLine x1 y1 x2 y2 color : salesman_ ((x2,y2):xs) l color


toSvg ::(Int,Int) ->  [[(Float,Float)]] -> String
toSvg (w,h) solutions = drawSvg . mkSvg w h  $ foldl f [] (zip solutions stdColor) where
        f z (solution,c) =  z ++ salesman solution c
     
