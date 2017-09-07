-- 48 min

data Boy = Matthew | Peter | Jack | Arnold | Carl
          deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
-- Matthew: Carl didn't do it, and neither did I.
accuses Matthew b = (b /= Carl) && b /= Matthew
-- Peter: It was Matthew or it was Jack.
accuses Peter b = (b == Matthew) || (b == Jack)
-- Jack: Matthew and Peter are both lying.
accuses Jack b = not (accuses Matthew b) && not (accuses Peter b)
-- Arnold: Matthew or Peter is speaking the truth, but not both.
-- 1 1
-- [1 0]
-- [0 1]
-- 0 0
accuses Arnold b = accuses Matthew b /= accuses Peter b
-- Carl: What Arnold says is not true.
accuses Carl b = not (accuses Arnold b)

accusers :: Boy -> [Boy]
accusers accused = [b | b <- boys, accuses b accused]

-- Guilty when 3 different accuse the accused
guilty, honest :: [Boy]
guilty = [b | b <- boys, length (accusers b) == 3]

-- Honest ones are the ones which accuse the result of guilty.
honest = [b | b <- boys, c <- guilty, accuses b c]
