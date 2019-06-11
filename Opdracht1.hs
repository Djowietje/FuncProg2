import Data.List (sortBy)
import Data.Ord (comparing)

type Lengte = Float
type Breedte = Float
type Straal = Float

-- Create Kleur Enum
data Kleur = ROOD|BLAUW|GEEL
    deriving (Eq, Show)

-- Create Geofig DataType
data GeoFig = Vierkant Lengte Kleur
    | Rechthoek Lengte Breedte Kleur
    | Driehoek Lengte Kleur
    | Cirkel Straal Kleur
        deriving Show

-- Define CalculateSurfaceArea Method
calculateSurfaceArea :: GeoFig->Float
calculateSurfaceArea (Vierkant l _) = l*l
calculateSurfaceArea (Rechthoek l b _) = l*b
calculateSurfaceArea (Driehoek l _) = l*l/2
calculateSurfaceArea (Cirkel s _) = s * s * pi

-- Define CalculateCircumference Method
calculateCircumference :: GeoFig->Float
calculateCircumference (Vierkant l _) = l * 4
calculateCircumference (Rechthoek l b _) = l*2 + b*2
calculateCircumference (Driehoek l _ ) = l * 3
calculateCircumference (Cirkel s _ ) = 2 * s * pi

-- Define a filter for GeoFig Types
shapeFilter :: String->GeoFig->Bool
shapeFilter "Vierkant" (Vierkant _ _) = True
shapeFilter "Rechthoek" (Rechthoek _ _ _ ) = True
shapeFilter "DrieHoek" (Driehoek _ _) = True
shapeFilter "Cirkel" (Cirkel _ _ ) = True
shapeFilter _ _ = False

-- Define a getter for Color on a GeoFig
getColor :: GeoFig->Kleur
getColor (Rechthoek _ _ c ) = c
getColor (Vierkant _ c ) = c
getColor (Driehoek _ c ) = c
getColor (Cirkel _ c ) = c

-- Define a filter for Geofig Colors
colorFilter :: String->GeoFig->Bool
colorFilter "ROOD" g@(_) = (getColor g) == ROOD
colorFilter "BLAUW" g@(_) = (getColor g) == BLAUW
colorFilter "GEEL" g@(_) = (getColor g) == GEEL

-- Create Some test objects.
square = Vierkant 10 ROOD
rectangle = Rechthoek 10 15 GEEL
triangle = Driehoek 10 BLAUW
circle = Cirkel 10 ROOD

-- Define a sort method on Surface
sortBySurface :: [GeoFig]->[GeoFig]
sortBySurface = sortBy( comparing (calculateSurfaceArea))

-- Define a sort method on Circumference
sortByCircumference :: [GeoFig]->[GeoFig]
sortByCircumference = sortBy( comparing (calculateCircumference))

-- Define a method to get biggest GeoFig based on SurfaceArea
getBiggestSurface :: [GeoFig]->GeoFig
getBiggestSurface x = last (sortBySurface x)

--Define a method to get biggest GeoFig based on Circumference
getBiggestCircumference :: [GeoFig]->GeoFig
getBiggestCircumference x = last (sortByCircumference x)

-- Create Random list of objects
objectList = [square, square, circle, triangle, circle, square, rectangle, rectangle]

-- Define a method to add a GeoFig to the objectList
addItemToList :: GeoFig->[GeoFig]->[GeoFig]
addItemToList fig list= do 
    list <- (fig : list)
    [list]

-- Define a method that returns the total surface area
calculateTotalArea :: [GeoFig] -> Float
calculateTotalArea [] = 0
calculateTotalArea [x] = calculateSurfaceArea x
calculateTotalArea (x:xs) = calculateSurfaceArea x + calculateTotalArea xs

-- Define a method that returns the percentage of surfaceArea.
relativeSizes :: [GeoFig] -> [Float]
relativeSizes xs = calcSizes xs
    where
        totalSize = calculateTotalArea xs
        calcSizes [] = [0]
        calcSizes [x] = [(calculateSurfaceArea x / totalSize) *100]
        calcSizes (x:xs) = (calculateSurfaceArea x / totalSize) * 100 : calcSizes xs

-- filter the random list for Squares
filteredShapeList = filter (shapeFilter "Vierkant") objectList
