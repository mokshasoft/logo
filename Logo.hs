module Logo
    ( genLogo
    ) where

import Graphics.Implicit
import Graphics.Implicit.Definitions

-- Golden ratio
gr = (1 + sqrt 5)/2

-- Configuration

fileName = "logo.svg"
nbrSections = 4
logoRadius = 200
sectionAngle = 2*pi/fromIntegral nbrSections
ccConnectorDistance = 2*logoRadius*sin(sectionAngle/2)
femaleOuterRadius = ccConnectorDistance*gr/(1 + 2*gr)
femaleWidth = femaleOuterRadius/(2 + 1/gr)
ringWidth = 2*femaleWidth/gr
maleRadius = femaleWidth

-- Generation

genLogo :: IO ()
genLogo =
    writeSVG 2 fileName $ logo nbrSections

logo :: Int -> SymbolicObj2
logo nbr =
    rotate (-pi/6) $ union $
        map (\s -> rotate (sectionAngle*fromIntegral s) section)
            [0..(nbr - 1)]

section :: SymbolicObj2
section =
    let
        s = femaleOuterRadius - femaleWidth/2
        t = logoRadius*sin(sectionAngle/2) - s
        r = logoRadius*cos(sectionAngle/2)
        beta = atan(t/r)
    in
        union
            -- add the male connector
            [ translate (logoRadius, 0) connMale
            -- add the female connector
            , rotate sectionAngle $ translate (logoRadius, 0) connFemale
            -- add the piece connecting them
            , ringSlice logoRadius ringWidth (beta + sectionAngle/2)
            ]

connMale :: SymbolicObj2
connMale =
    circle maleRadius

connFemale :: SymbolicObj2
connFemale =
    difference
        [ ring (femaleOuterRadius - femaleWidth/2) femaleWidth
        , translate (-logoRadius, 0) $ ringSlice logoRadius (ringWidth + femaleWidth/2) (pi/2)
        ]

pieSlice :: ℝ -> ℝ -> SymbolicObj2
pieSlice radius deg =
    difference
        [ circle radius
        , block radius
        , rotate (-(pi - deg)) $ block radius
        ]
  where
    block r = translate (-3*r/2, -r) $ rectR 0 (0, 0) (r*3, r)

ringSlice :: ℝ -> ℝ -> ℝ -> SymbolicObj2
ringSlice radius width deg =
    intersect
        [ ring radius width
        , pieSlice (radius*2) deg
        ]

ring :: ℝ -> ℝ -> SymbolicObj2
ring radius width =
    difference
        [ circle $ radius + width/2
        , circle $ radius - width/2
        ]
