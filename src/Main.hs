{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows #-}
-- |
-- Module      : Main
-- Description : Main module.
-- Copyright   : (c) Yale University, 2003
--
-- Author: Henrik Nilsson
module Main where

-- External imports
import           Data.Array
import           Data.Maybe    (isJust)
import           Data.Point2   (Point2 (..), point2Y)
import           FRP.Yampa
#if !WASM_BUILD
import qualified Graphics.HGL  as HGL
#else
import GHC.IO ( unsafePerformIO )
import WasmImports
#endif
import           System.Random (mkStdGen)

-- Internal imports
-- Temporary, just to make sure all modules compile.
import Animate
import Colors
import IdentityList
import Object
import ObjectBehavior
import Parser
import PhysicalDimensions
import RenderLandscape
import RenderObject
import WorldGeometry

type Score = Int

#if WASM_BUILD
actuate :: ReactHandle WinInput (Score, [ObsObjState]) -> Bool -> (Score, [ObsObjState]) -> IO Bool
actuate _ _ (score, ooss) = do
    clearCanvas 0 0 0
    renderScore score
    landscape
    renderObjects ooss
    return (score /= 0)

gameReactHandle :: ReactHandle WinInput (Score, [ObsObjState])
{-# NOINLINE gameReactHandle #-}
gameReactHandle = unsafePerformIO $ do
    let g = mkStdGen 123
    reactInit
        (pure $ WinInput 0.0 0.0 False)
        actuate
        (parseWinInput >>> restartingGame g)

-- Exported function to perform single game step, function is called from JS
foreign export ccall runGameStep :: Double -> Double -> Bool -> Double -> IO ()
runGameStep :: Double -> Double -> Bool -> Double -> IO ()
runGameStep x y pressed deltaTime = do
  _ <- react gameReactHandle (deltaTime, Just (WinInput x y pressed))
  return ()

main :: IO ()
main = return ()
#else

main :: IO ()
main = do
    let g = mkStdGen 123
    animate 20 "S P A C E   I N V A D E R S" worldSizeX worldSizeY
               -- Render
               (\(score, ooss) -> renderScore score
                                          `HGL.overGraphic` renderObjects ooss
                                          `HGL.overGraphic` landscape)
               -- Text console output
               (\_ -> [])
               -- The actual game (see note about misnomer)
               (parseWinInput >>> restartingGame g)

#endif
-- Change name to "game"? What's now "game" would become "gameRound".
-- What about "game'"?
restartingGame :: RandomGen g => g -> SF GameInput (Int, [ObsObjState])
restartingGame g = rgAux g nAliens0 vydAlien0 0
    where
        nAliens0 = 2
        vydAlien0 = -10

        rgAux g nAliens vydAlien score =
            switch (game g' nAliens vydAlien score) $ \status ->
            case status of
                Left score' ->
                  -- Next level
                  rgAux g'' (nAliens + 1) (vydAlien - 10) score'

                Right _ ->
                  -- Game over
                  rgAux g'' nAliens0 vydAlien0 0
            where
                (g', g'') = split g


-- How to arrange for splitting of g in general if we have spawning of aliens
-- "inside" the game? The spawner should
-- keep control over the supply of random generators.

game :: RandomGen g => g -> Int -> Velocity -> Score ->
        SF GameInput ((Score, [ObsObjState]), Event (Either Score Score))
game g nAliens vydAlien score0 = proc gi -> do
    -- One could argue that feeding back only the ObsObjState part would
    -- make things a little smoother. But possibly somewhat less general.
    -- iPre not needed in the feedback path as long as dpSwitch is used,
    -- but there are many side conditions related to the game. E.g.
    -- the parts of the fed-back output used for hit detection not depending
    -- on the result of the hit detection and so on. So for robustness,
    -- maybe safest to leave the delay in.
    --
    -- BUT! We cannot have BOTH dpSwitch and iPre! That will cause e.g.
    -- hitting shots to be "seen" twice, due to a "double delay", and
    -- that in turn will inflict double dammage.
    --
    -- We could do score keeping *in this case* by simply *continusouly*
    -- counting the number of aliens. This count can be used both to detect
    -- a new round, and for computing the score as nAliens - alienCount.
    -- But this does not exactly invite to more elaborate scoring schemes.
    -- it would also be difficult to have different kinds of aliens of
    -- varying value, since the natural way to do that is to look at the
    -- actual object that is being removed in order to figure out its value,
    -- rather than tryingto figure out a score indirectly through the
    -- absence of objects!
    rec
        oos  <- game' objs0  -< (gi, oos) -- oosp
        -- oosp <- iPre emptyIL -< oos
    score    <- accumHold score0 -< aliensDied oos
    gameOver <- edge             -< alienLanded oos
    newRound <- edge             -< noAliensLeft oos
    returnA -<
      ( (score, map ooObsObjState (elemsIL oos)),
        (newRound `tag` (Left score)) `lMerge` (gameOver `tag` (Right score))
      )
    where
        objs0 = listToIL
                  (gun (Point2 0 50) : mkAliens g (worldXMin + d) 900 nAliens)

         -- Evenly spaced aliens
        d = (worldXMax - worldXMin) / fromIntegral (nAliens + 1)


        mkAliens g x y n | n > 0 = alien g' (Point2 x y) vydAlien
                                   : mkAliens g'' (x + d) y (n - 1)
                        where (g', g'') = split g
        mkAliens _ _ _ 0 = []
        mkAliens _ _ _ _ = []

        aliensDied :: IL ObjOutput -> Event (Score -> Score)
        aliensDied oos =
            fmap (\es -> (+length es))
                 (catEvents (map ooKillReq (findAllIL isAlien' oos)))
            where
                isAlien' (_, ObjOutput {ooObsObjState = oos}) = isAlien oos

        alienLanded :: IL ObjOutput -> Bool
        alienLanded oos = isJust (findIL isLanded oos)
            where
                isLanded (_, ObjOutput {ooObsObjState = oos}) =
                    isAlien oos && point2Y (oosPos oos) <= 0

        noAliensLeft :: IL ObjOutput -> Bool
        noAliensLeft oos = null (findAllIL isAlien' oos)
            where
                isAlien' (_, ObjOutput {ooObsObjState = oos}) = isAlien oos

        -- dpSwitch for now. This makes kill/spawn events observable,
        -- and allows feedback without iPre *in this case*, since only the
        -- switching depends on the feedback, and hit detection does not
        -- depend on any part of the fed-back output that in turn depends on
        -- the hit detection. But this is a really fragile property!
        -- Not that notYet is needed regardless of whether pSwictch or
        -- dpSwitch is used.

        -- game' :: IL Object -> SF (GameInput, IL ObjOutput) (IL ObjOutput)
        -- game' objs = dpSwitch route
        --                       objs
        --                       (arr killOrSpawn >>> notYet)
        --                       (\sfs' f -> game' (f sfs'))

        -- Slightly more efficient, and maybe clearer?
        game' :: IL Object -> SF (GameInput, IL ObjOutput) (IL ObjOutput)
        game' objs = dpSwitch route
                              objs
                              (noEvent --> arr killOrSpawn)
                              (\sfs' f -> game' (f sfs'))
        -- IPerez: What's the difference between objs and sfs'?
        -- Are sfs' the objs' continuations?


        route :: (GameInput, IL ObjOutput) -> IL sf -> IL (ObjInput, sf)
        route (gi,oos) objs = mapIL routeAux objs

            where
                routeAux (k, obj) =
                    (ObjInput {oiHit = if k `elem` hs
                                       then Event ()
                                       else noEvent,
                               oiGameInput = gi},
                     obj)
                hs = hits (assocsIL (fmap ooObsObjState oos))


        -- This could be refined to full-fledged collision detection with
        -- computation of proper impulses, and merging (adding) of multiple
        -- impulses influencing a single object.
        hits :: [(ILKey, ObsObjState)] -> [ILKey]
        hits kooss = concat (hitsAux kooss)
            where
                hitsAux [] = []
                hitsAux ((k,oos):kooss) =
                    [ [k, k'] | (k', oos') <- kooss, oos `hit` oos' ]
                    ++ hitsAux kooss

                oos1 `hit` oos2
                    | isMissile oos1 && isAlien oos2
                      || isAlien oos1 && isMissile oos2 = oos1 `colliding` oos2
                    | otherwise = False

        killOrSpawn :: (a, IL ObjOutput) -> (Event (IL Object -> IL Object))
        killOrSpawn (_, oos) =
            foldl (mergeBy (.)) noEvent es
            where
                es :: [Event (IL Object -> IL Object)]
                es = [ mergeBy (.)
                               (ooKillReq oo `tag` (deleteIL k))
                               (fmap (foldl (.) id . map insertIL_)
                                     (ooSpawnReq oo))
                     | (k,oo) <- assocsIL oos ]

#if !WASM_BUILD
renderScore :: Score -> HGL.Graphic
renderScore score =
    HGL.withTextColor (colorTable ! White) $
    HGL.withTextAlignment (HGL.Left', HGL.Top) $
    HGL.text gp (show score)
    where
        gp = position2ToGPoint (Point2 worldXMin worldYMax)
#else
renderScore :: Score -> IO ()
renderScore score = do
    setFontHelper "20px serif"
    fillStyle 180 230 200
    fillTextHelper ("Score: "++show score) 10 40 300
#endif