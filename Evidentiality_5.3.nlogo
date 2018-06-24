;; Changes from v.4.0.:
;; - prone-to-mess-up and number-non-trustworthy are gone; reliability introduced instead
;; - added switch "allow-scilence"
;; - new gains

;; TODO да проверя може ли close-to-speaker да се изведе като репортерче

turtles-own [information ;if the turtle has been informed about the event (either by witnessing it or by heresay). Not to be confused with belief.
             rank ;people are ranked by how 'close' to the event their information source is; witnesses have rank 0, the people they've talked to have rank 1 and so on.
             close-to-speaker ;if a turtle is close enough to a possible speaker to be engaged in a conversation by them
             communication-strategy ;the probabilities to choose one or another message based on a belief and vice versa
             pure-local-strategy; how close their strategy is to a pure one
             pure-strategy;
             belief ;either a true (1) or a false (-1) belief relative to the event that happened. If no belief for the event, value 0.
             message ;either "", "basic message", "firsthand message" or "hearsay message"
             to-die ;true if marked to die on next turn, false by default
             reputation; 0 to 100
]

globals [current-state-of-the-model ;tell the user what's happening
         witnesses ;how many people will witness each event
         hearers ;the set of the people that have heard from somebody about the event
         believes; a list with the counts of the agents with true believes, no believes and false believes about the event
         moves;
]

;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup, Step and Go ;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  set current-state-of-the-model "Nothing has happened yet."
  ask patches [set pcolor white ]
  create-turtles population
  set hearers turtle-set nobody
  set witnesses turtle-set nobody
  setup-turtles
  set believes (list 0 0 0) ;number of turtles with true, none and false believes
  set moves (list 0 0 0 0 0 0 0 0 0 0 0 0) ;moves in each turn see pure strategy for the orther of the moves
  reset-ticks
end


to step
 ifelse (not any? turtles with [information = TRUE])
 [ set moves (list 0 0 0 0 0 0 0 0 0 0 0 0)
   witness-event ]
 [ ifelse not any? turtles with [information = FALSE]
    [ reset ]
    [ talk ]
 ]
end

to go
  ifelse not (count turtles = count witnesses)
  [ step ]
  [ stop ]
end

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

;; Setup turtles

to setup-turtles
  ask turtles
  [ setxy random-pxcor random-pycor
    set reputation 50
    set shape "circle"
    set-size
    set information FALSE
    set rank 0
    set close-to-speaker FALSE
    set communication-strategy setup-communication-strategy
    set belief 0
    set-color
    set message ""
    set pure-local-strategy setup-pure-local-strategy
    set pure-strategy setup-pure-strategy
    set to-die false
  ]
end

; color the turtles according to their communication strategies
to set-color
  ;set color rgb red-value green-value blue-value
  set color (list red-value green-value blue-value transparency)
end

; the size of the turtle depends on it's reputation, but is not less than 0.25
to set-size
  let turtle-size 0.8
  if show-reputation
  [ set turtle-size 0.25 + sqrt (reputation / 100)]
  set size turtle-size
end

; the redder the more inclined to use the basic message
to-report red-value
  report color-value 0
end

; the greener, the more inclined to use the lexically marked message
to-report green-value
  report color-value 1
end

; the bluer, the more inclined to use the grammatically marked message
to-report blue-value
  report color-value 2
end

to-report transparency
  let total 1
  ifelse (hearer-strategy-for = "basic message")
  [ set total sum (item 0 item 1 communication-strategy)
    report 50 + (205 / total) * item 0 item 0 item 1 communication-strategy
  ]
  [ ifelse (hearer-strategy-for = "firsthand message")
    [ set total sum (item 1 item 1 communication-strategy)
      report 50 + (205 / total) * item 0 item 1 item 1 communication-strategy]
    [ ifelse (hearer-strategy-for = "hearsay message")
      [ set total sum (item 2 item 1 communication-strategy) ;hearsay
        report 50 + (205 / total) * item 0 item 2 item 1 communication-strategy
      ]
      [ report 255 ]
    ]
  ]
end

to-report color-value [index]
  let total 1
  ifelse (speaker-strategy-for = "witnessed events")
  [ set total sum (item 0 item 0 communication-strategy)
    report (255 / total) * item index item 0 item 0 communication-strategy
  ]
  [ ifelse (speaker-strategy-for = "reported information")
    [ set total sum (item 1 item 0 communication-strategy)
      report (255 / total) * item index item 1 item 0 communication-strategy
    ]
    [ ifelse (speaker-strategy-for = "doubted information")
      [ set total sum (item 2 item 0 communication-strategy)
        report (255 / total) * item index item 2 item 0 communication-strategy
      ]
      [report 0 ]
    ]
  ]
end

;; Select witnesses

to witness-event
  set current-state-of-the-model "Something happened."
  let i 0
  ask n-of number-witnesses turtles
    [ set information TRUE
      set shape "square"
      form-a-belief
  ]
  set witnesses (turtle-set turtles with [information = TRUE])
  set current-state-of-the-model "Some people saw what happened."
end

to form-a-belief
ifelse reliability = 1
    [ set belief 1 ]
    [ let i random-float 1
      ifelse i < reliability
      [ set belief 1 ]
      [ set belief -1 ]
    ]
end

;; Talk

to talk
  set current-state-of-the-model "Information spreads."
  let possible-speakers turtles with [information = TRUE]
  let possible-hearers turtles with [information = FALSE]
  let any-hearers-found False
  while [any? possible-speakers and any? possible-hearers]
  [ set any-hearers-found False
    let max-distance 1
    while [not any-hearers-found]
    [ foreach sort-on [rank] possible-speakers with [not (message = "scilence")]
      [ speaker ->
        ask speaker [ask possible-hearers in-radius max-distance [set close-to-speaker TRUE]]
        let close-ones sort possible-hearers with [close-to-speaker = TRUE]
        ; the close ones may decide they don't want to listen to this speaker if he has low reputation
        if not empty? close-ones
        [ foreach close-ones
          [ one ->
            let rand random 100
            if rand > [reputation] of speaker
            [ ask one [ set close-to-speaker FALSE ]
              set close-ones remove one close-ones
            ]
          ]
        ]
        if not empty? close-ones
        [ set any-hearers-found TRUE
          if ([message] of speaker = "")
          [ ask speaker [choose-message] ]
          ask speaker [record-s-move]
          if not ([message] of speaker = "scilence")
          [ let hearer one-of close-ones
            set hearers (turtle-set hearers hearer)
            let s-rank [rank] of speaker
            ask hearer
            [ believe speaker
              create-link-from speaker
              [ set shape "small arrow"
                if [belief] of source speaker = -1
                [ set shape "small arrow dotted" ] ;dotted line for untrue statement

                let link-color [0 0 0 255]

                if show-messages
                [ ifelse [message] of speaker = "basic message"
                  [ set link-color [255 0 0 255] ]
                  [ ifelse [message] of speaker = "first hand message"
                    [ set link-color [0 255 0 255] ]
                    [ if [message] of speaker = "hearsay message"
                      [ set link-color [0 0 255 255] ]
                    ]
                  ]
                ]

                if ([belief] of hearer = 0)
                [ set link-color replace-item 3 link-color 75 ] ;transparent line for not believed statement

                set color link-color

                set thickness 0.1;0.25 - (s-rank / 20) ;the link's thikness is max. 0.25 and get's lowered by 0.05 for further ranks
              ]
              set information TRUE
              set rank s-rank + 1
            ]
            set possible-hearers turtles with [information = FALSE]
            set possible-speakers turtles with [information = TRUE]
            layout
          ]
          foreach close-ones
          [ one ->
            ask one [set close-to-speaker FALSE]
          ]
        ]
      ]
      set max-distance max-distance + 1
    ]
  ]
  set current-state-of-the-model "Everybody knows what happened."
  set believes (list count turtles with [belief = 1] count turtles with [belief = 0]  count turtles with [belief = -1])
end

to choose-message
  let i 1
  if member? self witnesses
  [ set i 0 ]
  if (belief = 0)
  [ set i 2 ]
  let r random sum (item i item 0 communication-strategy)
  ifelse r < item 0 item i item 0 communication-strategy
  [ set message "basic message" ]
  [ ifelse r < (item 0 item i item 0 communication-strategy + item 1 item i item 0 communication-strategy)
    [ set message "first hand message" ]
    [ ifelse r < (item 0 item i item 0 communication-strategy + item 1 item i item 0 communication-strategy + item 2 item i item 0 communication-strategy)
      [ set message "hearsay message" ]
      [ set message "scilence" ]
    ]
  ]
end

to believe [a-speaker]
let t random-float 1
  let i [message-index] of a-speaker
  let trust-prob (1 / sum item i item 1 communication-strategy) * item 0 item i item 1 communication-strategy
  ifelse t <= trust-prob
  [ let mess random-float 1
    ifelse mess > reliability
    [ set belief -1 ]
    [ ifelse not ([belief] of a-speaker = 0)
      [ set belief [belief] of a-speaker ]
      [ set belief [belief] of source a-speaker ]
    ]
  ]
   [ set belief 0 ]
end

to-report source [a-speaker]
  let the-source a-speaker
  while [[belief] of the-source = 0]
  [ ask the-source [set the-source [other-end] of one-of [my-in-links] of the-source]]
  report the-source
end

to-report message-index
  ifelse (message = "basic message")
  [ report 0 ]
  [ ifelse (message = "first hand message")
    [ report 1 ]
    [ ifelse (message = "hearsay message")
      [ report 2 ]
      [if (message = "scilence")
        [report 3]
      ]
    ]
  ]
  report -1
end

to-report utterance-cost
  ifelse (message = "basic message")
  [ report 2 ]
  [ ifelse (message = "first hand message")
    [ report 3 ]
    [ if (message = "hearsay message")
      [ report 3 ]
    ]
  ]
end

to-report reputation-bet
  ifelse (message = "first hand message")
  [ report reputation-bet-marked ]
  [ ifelse (message = "basic message")
    [ report reputation-bet-unmarked ]
    [ report 0 ]
  ]
end


;; to cite the Preferential attachment model
to layout
  ;; the more turtles we have to fit into the same amount of space,
  ;; the smaller the inputs to layout-spring we'll need to use
  let factor 1 ;; in case there are no hearers
  if any? hearers [set factor sqrt count hearers]
  ;; numbers here are arbitrarily chosen for pleasing appearance
  layout-spring hearers links (1 / factor) 1 (1 / factor)
  display  ;; for smooth animation
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask hearers [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end
;; end of Preferential Attachment borowing

to reset
  ;wrap up the old rummor
  set current-state-of-the-model "People lost interest in this topic."
  record-h-moves
  let max-rank max [rank] of turtles
  while [max-rank > 0]
  [ while [any? turtles with [rank = max-rank] ]
    [ ask one-of turtles with [rank = max-rank]
      [ set rank 0
        update-full-strategy ;only non-witnesses update their strategy
        ask my-in-links [ die ]
      ]
      layout
    ]
    set max-rank max-rank - 1
  ]
  ask turtles [ set-color set-size ]
  ;start the new event and the new rummor
  set hearers turtle-set nobody
  set current-state-of-the-model "Something happened."
  ask turtles
  [ set information FALSE
    if (reputation = 0) [ set to-die true ]
    set message ""
    set belief 0
  ]
  set witnesses turtle-set nobody
  if ( black-magic = true)
  [ ask turtles with [ to-die = true ] [ die ] ]
  ask turtles with [to-die = true] [ die ]
  if (count turtles < population and any? turtles with [reputation = 100])
  [ ask one-of turtles with [reputation = 100] [hatch 1 [ set reputation 50 set-size set shape "circle" ]]]
  ask turtles [set shape "circle" update-pure-local-strategy]
  tick
end

to update-reputation
  if (message = "basic message" or message = "first hand message")
  [ ifelse belief = -1
    [ set reputation max list 0 (reputation - (reputation-bet))]
    [ if belief = 1
      [ set reputation min list 100 (reputation + (reputation-bet))]
    ]
  ]
end

to-report setup-communication-strategy
  report list
              (list                 ; speaker's strategy
                    (list 100 100 0 0) ; urn for when a witness, with balls for basic, first hand, reported and no message
                    (list 100 0 100 0) ; urn for when informed by others and believed them, with balls for basic, first hand, reported and no message
                    (list 0 0 100 100)); urn for when informed by others and disbelieved them, with balls for basic, first hand, reported and no message
              (list                 ; hearer's strategy
                    list 100 100  ; urn with balls for accepting or rejecting the information when presented with the basic message
                    list 100 100  ; urn with balls for accepting or rejecting the information when presented with the firsthand information message
                    list 100 100) ; urn with balls for accepting or rejecting  the information when presented with the reported information message
end

to update-full-strategy
  let the-source [other-end] of one-of my-in-links
  let ultimate-source source self

  let s [message-index] of the-source

  let b 0 ;if the rumor has(!) been believed

  let hearer-utility 0

  ifelse (belief = 0) ;the agent hasn't believed the message
  [ set b 1 ]  ; if the agent hasn't believed the message, the gain remains zero (nothing for the hearer, no cooperation gain for the speaker)
  [ ifelse belief = -1
    [ if ([belief] of ultimate-source = -1) ;else, it's the hearer's fault not the message's
      [set hearer-utility -10 ] ; if sth false is believed, it's not cool
    ]
    [ if belief = 1
      [set hearer-utility 10] ; if sth true is believed, it's great
    ]
  ]
  ;update the hearer strategy
  set communication-strategy replace-item 1 communication-strategy
                             replace-item s item 1 communication-strategy
                             replace-item b item s item 1 communication-strategy
                             max list 1 (item b item s item 1 communication-strategy + hearer-utility)

  ;update source's strategy
  if ( s >= 0 and not ( belief = -1 and [belief] of ultimate-source = 1)) ; if the hearer didn't get the message right, the speaker's strategy is not updated
  [ let cost [utterance-cost] of the-source ;

    ; if the speaker spreads true information, his reputation raises, else - it goes down
    if [belief] of ultimate-source = -1 [ set cost cost + [reputation-bet] of the-source ]
    if [belief] of ultimate-source = 1 [ set cost cost - [reputation-bet] of the-source ] ; the bet compencates the utterance cost

    ;if the message is the hearsay one, then the gain/loss is less for the speaker (the intention is to provide the hearer with options to choose from, not to persuade them)

    let t 0 ;type of the source (witness or what)

    if not member? the-source witnesses
    [ ifelse ([belief] of the-source = 0)
      [ set t 2 ] ;heard but didn't believe
      [ set t 1 ] ;heard and believed
    ]

    let perlocutionary-gain 0;

    ifelse (not (t = 2) and not (belief = 0)) ; the speaker wanted and succeeded forwarding their belief
    [ set perlocutionary-gain 10
    ]
    [ if (t = 2) ; the speaker just wanted to give the hearer options, whether or not he believed them is irrelevant
      [ set perlocutionary-gain 3 ]
    ]

    let utility hearer-utility + perlocutionary-gain - cost

    ask the-source
    [ if (item s item t item 0 communication-strategy + utility > 100000) [show "Big number!"]
      set communication-strategy replace-item 0 communication-strategy
                               replace-item t item 0 communication-strategy
                               replace-item s item t item 0 communication-strategy
                               min list 10000 (max list 1 (item s item t item 0 communication-strategy + utility))
      update-reputation
      ; if the speaker has lied (said sth untrue without marking it as hearsay, they die
      if (black-magic = true and not (s = 2) and (belief = -1)) [ set to-die true]
    ]
  ]

end

to-report setup-pure-local-strategy
  report (list FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE)
end

to-report setup-pure-strategy
  report (list FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE)
end

to update-pure-local-strategy
  let t1 (number-witnesses / population)
  let t2 (1 - t1) * ((item 0 item 2 item 1 communication-strategy) / sum(item 2 item 1 communication-strategy))
  let t3 (1 - t1 - t2)

  let x1 (item 0 item 0 item 0 communication-strategy) / sum(item 0 item 0 communication-strategy)
  let x2 (item 1 item 0 item 0 communication-strategy) / sum(item 0 item 0 communication-strategy)
  let x3 (item 2 item 0 item 0 communication-strategy) / sum(item 0 item 0 communication-strategy)
  let x4 (item 3 item 0 item 0 communication-strategy) / sum(item 0 item 0 communication-strategy)
  let y1 (item 0 item 1 item 0 communication-strategy) / sum(item 1 item 0 communication-strategy)
  let y2 (item 1 item 1 item 0 communication-strategy) / sum(item 1 item 0 communication-strategy)
  let y3 (item 2 item 1 item 0 communication-strategy) / sum(item 1 item 0 communication-strategy)
  let y4 (item 3 item 1 item 0 communication-strategy) / sum(item 1 item 0 communication-strategy)
  let z1 (item 0 item 2 item 0 communication-strategy) / sum(item 2 item 0 communication-strategy)
  let z2 (item 1 item 2 item 0 communication-strategy) / sum(item 2 item 0 communication-strategy)
  let z3 (item 2 item 2 item 0 communication-strategy) / sum(item 2 item 0 communication-strategy)
  let z4 (item 3 item 2 item 0 communication-strategy) / sum(item 2 item 0 communication-strategy)

  let xyz (list x1 x2 x3 x4 y1 y2 y3 y4 z1 z2 z3 z4)
  let t-list (list t1 t2 t3)

  let h 1 ; heiligen distance, start with maximum
  let th 0.3 ; threshhold


  ;check for S-AAA
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) t2) (list (t2 * y3) 0)
                         (list (t3 * z3) t3) (list (t3 * z4) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 0 pure-strategy TRUE]
  [ set pure-strategy replace-item 0 pure-strategy FALSE]

  ;check for S-AAB
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) t2)(list (t2 * y3) 0)
                         (list (t3 * z3) 0) (list (t3 * z4) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 1 pure-strategy TRUE]
  [ set pure-strategy replace-item 1 pure-strategy FALSE]

  ;check for S-ABA
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) 0)(list (t2 * y3) t2)
                         (list (t3 * z3) t3) (list (t3 * z4) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 2 pure-strategy TRUE]
  [ set pure-strategy replace-item 2 pure-strategy FALSE]

  ;check for S-ABB
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) 0) (list (t2 * y3) t2)
                         (list (t3 * z3) 0) (list (t3 * z4) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 3 pure-strategy TRUE]
  [ set pure-strategy replace-item 3 pure-strategy FALSE]

  ;check for S-BAA
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) t2) (list (t2 * y3) 0)
                         (list (t3 * z3) t3) (list (t3 * z4) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 4 pure-strategy TRUE]
  [ set pure-strategy replace-item 4 pure-strategy FALSE]

  ;check for S-BAB
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) t2) (list (t2 * y3) 0)
                         (list (t3 * z3) 0) (list (t3 * z4) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 5 pure-strategy TRUE]
  [ set pure-strategy replace-item 5 pure-strategy FALSE]

  ;check for S-BBA
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) 0) (list (t2 * y3) t2)
                         (list (t3 * z3) t3) (list (t3 * z4) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 6 pure-strategy TRUE]
  [ set pure-strategy replace-item 6 pure-strategy FALSE]

  ;check for S-BBB
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) 0) (list (t2 * y3) t2)
                         (list (t3 * z3) 0) (list (t3 * z4) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 7 pure-strategy TRUE]
  [ set pure-strategy replace-item 7 pure-strategy FALSE]

  ;check for S1A (witnessed - first hand)
  set h heillinger (list (list x1 0) (list x2 1) (list x3 0) (list x4 0))
  ifelse h <= th
  [set pure-local-strategy replace-item 0 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 0 pure-local-strategy FALSE]

  ;check for S1B (witnessed - basic)
  set h heillinger (list (list x1 1) (list x2 0) (list x3 0) (list x4 0))

  ifelse h <= th
  [set pure-local-strategy replace-item 1 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 1 pure-local-strategy FALSE]

  ;check for S2A (heard and believed - basic)
  set h heillinger (list (list y1 1) (list y2 0) (list y3 0) (list y4 0))

  ifelse h <= th
  [set pure-local-strategy replace-item 2 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 2 pure-local-strategy FALSE]

  ;check for S2B (heard and believed - hearsay)
  set h heillinger (list (list y1 0) (list y2 0) (list y3 1) (list y4 0))

  ifelse h <= th
  [set pure-local-strategy replace-item 3 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 3 pure-local-strategy FALSE]

  ;check for S3A (heard and disbelieved - hearsay)
  set h heillinger (list (list z1 0) (list z2 0) (list z3 1) (list z4 0))

  ifelse h <= th
  [set pure-local-strategy replace-item 4 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 4 pure-local-strategy FALSE]

  ;check for S3B (heard and disbelieved - none)
  set h heillinger (list (list z1 0) (list z2 0) (list z3 0) (list z4 1))

  ifelse h <= th
  [set pure-local-strategy replace-item 5 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 5 pure-local-strategy FALSE]


  ; EXPERIMENTAL
  set y1 (item 0 item 1 item 0 communication-strategy + item 0 item 2 item 0 communication-strategy) / (sum(item 1 item 0 communication-strategy) + sum(item 2 item 0 communication-strategy))
  set y2 (item 1 item 1 item 0 communication-strategy + item 1 item 2 item 0 communication-strategy) / (sum(item 1 item 0 communication-strategy) + sum(item 2 item 0 communication-strategy))
  set y3 (item 2 item 1 item 0 communication-strategy + item 2 item 2 item 0 communication-strategy) / (sum(item 1 item 0 communication-strategy) + sum(item 2 item 0 communication-strategy))
  set y4 (item 3 item 1 item 0 communication-strategy + item 3 item 2 item 0 communication-strategy) / (sum(item 1 item 0 communication-strategy) + sum(item 2 item 0 communication-strategy))

  set h heillinger (list (list y1 1) (list y2 0) (list y3 0) (list y4 0))

  ifelse h <= th
  [set pure-local-strategy replace-item 12 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 12 pure-local-strategy FALSE]

  set h heillinger (list (list y1 0) (list y2 0) (list y3 1) (list y4 0))

  ifelse h <= th
  [set pure-local-strategy replace-item 13 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 13 pure-local-strategy FALSE]

  set h heillinger (list (list y1 0) (list y2 0) (list y3 0) (list y4 1))

  ifelse h <= th
  [set pure-local-strategy replace-item 14 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 14 pure-local-strategy FALSE]

  ; For the hearer's strategies:

  set x1 (item 0 item 0 item 1 communication-strategy) / sum(item 0 item 1 communication-strategy)
  set x2 (item 1 item 0 item 1 communication-strategy) / sum(item 0 item 1 communication-strategy)
  set y1 (item 0 item 1 item 1 communication-strategy) / sum(item 1 item 1 communication-strategy)
  set y2 (item 1 item 1 item 1 communication-strategy) / sum(item 1 item 1 communication-strategy)
  set z1 (item 0 item 2 item 1 communication-strategy) / sum(item 2 item 1 communication-strategy)
  set z2 (item 1 item 2 item 1 communication-strategy) / sum(item 2 item 1 communication-strategy)

  set t1  0.5; roughly the chance for basic message
  set t2 (number-witnesses / 2) / population ; roughly the chance for first hand message
  set t3 1 - (t1 + t2) ; roughly the chance for hearsay message

  ;check for H-AAA
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) t2) (list (t2 * y2) 0)
                         (list (t3 * z1) t3) (list (t3 * z2) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 8 pure-strategy TRUE]
  [ set pure-strategy replace-item 8 pure-strategy FALSE]

  ;check for H-AAB
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) t2) (list (t2 * y2) 0)
                         (list (t3 * z1) 0) (list (t3 * z2) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 9 pure-strategy TRUE]
  [ set pure-strategy replace-item 9 pure-strategy FALSE]

    ;check for H-ABA
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) 0) (list (t2 * y2) t2)
                         (list (t3 * z1) t3) (list (t3 * z2) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 10 pure-strategy TRUE]
  [ set pure-strategy replace-item 10 pure-strategy FALSE]

  ;check for H-ABB
  set h heillinger (list (list (t1 * x1) t1) (list (t1 * x2) 0)
                         (list (t2 * y1) 0) (list (t2 * y2) t2)
                         (list (t3 * z1) 0) (list (t3 * z2) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 11 pure-strategy TRUE]
  [ set pure-strategy replace-item 11 pure-strategy FALSE]


  ;check for H-BAA
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) t2) (list (t2 * y2) 0)
                         (list (t3 * z1) t3) (list (t3 * z2) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 12 pure-strategy TRUE]
  [ set pure-strategy replace-item 12 pure-strategy FALSE]

  ;check for H-BAB
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) t2) (list (t2 * y2) 0)
                         (list (t3 * z1) 0) (list (t3 * z2) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 13 pure-strategy TRUE]
  [ set pure-strategy replace-item 13 pure-strategy FALSE]

    ;check for H-BBA
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) 0) (list (t2 * y2) t2)
                         (list (t3 * z1) t3) (list (t3 * z2) 0))
  ifelse h <= th
  [ set pure-strategy replace-item 14 pure-strategy TRUE]
  [ set pure-strategy replace-item 14 pure-strategy FALSE]

  ;check for H-BBB
  set h heillinger (list (list (t1 * x1) 0) (list (t1 * x2) t1)
                         (list (t2 * y1) 0) (list (t2 * y2) t2)
                         (list (t3 * z1) 0) (list (t3 * z2) t3))
  ifelse h <= th
  [ set pure-strategy replace-item 15 pure-strategy TRUE]
  [ set pure-strategy replace-item 15 pure-strategy FALSE]



  ;check for H1A (basic - believed)
  set h heillinger (list (list x1 1) (list x2 0))
  ifelse h <= th
  [set pure-local-strategy replace-item 6 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 6 pure-local-strategy FALSE]
  ;check for H1B (basic - disbelieved)
  set h heillinger (list (list x1 0) (list x2 1))
  ifelse h <= th
  [set pure-local-strategy replace-item 7 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 7 pure-local-strategy FALSE]
  ;check for H2A (first hand - believed)
  set h heillinger (list (list y1 1) (list y2 0))
  ifelse h <= th
  [set pure-local-strategy replace-item 8 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 8 pure-local-strategy FALSE]
  ;check for H2B (first hand - disbelieved)
  set h heillinger (list (list y1 0) (list y2 1))
  ifelse h <= th
  [set pure-local-strategy replace-item 9 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 9 pure-local-strategy FALSE]
  ;check for H3A (hearsay - believed)
  set h heillinger (list (list z1 1) (list z2 0))
  ifelse h <= th
  [set pure-local-strategy replace-item 10 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 10 pure-local-strategy FALSE]
  ;check for H3B (hearsay - disbelieved)
  set h heillinger (list (list z1 0) (list z2 1))
  ifelse h <= th
  [set pure-local-strategy replace-item 11 pure-local-strategy TRUE]
  [set pure-local-strategy replace-item 11 pure-local-strategy FALSE]
end

to-report heillinger [alist]
  let the-sum 0
  foreach alist
  [ pair ->
    set the-sum the-sum + sqrt(item 0 pair * item 1 pair)
  ]
  report sqrt(1 - the-sum)
 end

to record-s-move
  let w member? self witnesses
  let b belief
  let m message-index
  ifelse w
  [ ifelse m = 1
    [set moves replace-item 0 moves (item 0 moves + 1)]
    [ if m = 0
      [set moves replace-item 1 moves (item 1 moves + 1)]
    ]
  ]
  [ ifelse not (b = 0)
    [ ifelse m = 0
      [ set moves replace-item 2 moves (item 2 moves + 1)]
      [ set moves replace-item 3 moves (item 3 moves + 1)]
    ]
    [ ifelse m = 2
      [ set moves replace-item 4 moves (item 4 moves + 1)]
      [ set moves replace-item 5 moves (item 5 moves + 1)]
    ]
  ]
end

to record-h-moves
  set moves replace-item 6 moves count links with [color = [255 0 0 255]]
  set moves replace-item 7 moves count links with [color = [255 0 0 75]]
  set moves replace-item 8 moves count links with [color = [0 255 0 255]]
  set moves replace-item 9 moves count links with [color = [0 255 0 75]]
  set moves replace-item 10 moves count links with [color = [0 0 255 255]]
  set moves replace-item 11 moves count links with [color = [0 0 255 75]]
end
@#$#@#$#@
GRAPHICS-WINDOW
224
16
706
499
-1
-1
14.364
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
14
15
69
48
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
14
123
205
156
number-witnesses
number-witnesses
1
population / 2
1.0
1
1
NIL
HORIZONTAL

BUTTON
152
16
207
49
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
81
15
139
48
Step
step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
14
51
131
84
population
population
10
200
200.0
10
1
NIL
HORIZONTAL

SLIDER
14
87
131
120
reliability
reliability
0.5
1
0.85
0.05
1
NIL
HORIZONTAL

PLOT
722
473
1284
606
Believes
time
agents
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"none" 1.0 1 -7500403 true "" "plot sum believes"
"true" 1.0 1 -13345367 true "" "plot item 0 believes + item 2 believes"
"false" 1.0 1 -2674135 true "" "plot item 2 believes "

CHOOSER
12
281
210
326
speaker-strategy-for
speaker-strategy-for
"witnessed events" "reported information" "doubted information" "off"
1

CHOOSER
12
329
209
374
hearer-strategy-for
hearer-strategy-for
"basic message" "firsthand message" "hearsay message" "off"
3

PLOT
1002
18
1280
155
Pure Local H-Strategies
time
agents
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"H1A" 1.0 0 -2674135 true "" "plot count turtles with [item 6 pure-local-strategy = TRUE]"
"H1B" 1.0 0 -1069655 true "" "plot count turtles with [item 7 pure-local-strategy = TRUE]"
"H2A" 1.0 0 -13840069 true "" "plot count turtles with [item 8 pure-local-strategy = TRUE]"
"H2B" 1.0 0 -5509967 true "" "plot count turtles with [item 9 pure-local-strategy = TRUE]"
"H3A" 1.0 0 -13791810 true "" "plot count turtles with [item 10 pure-local-strategy = TRUE]"
"H3B" 1.0 0 -5516827 true "" "plot count turtles with [item 11 pure-local-strategy = TRUE]"

SWITCH
12
377
210
410
show-messages
show-messages
1
1
-1000

PLOT
722
17
998
155
Pure Local S-Strategies
time
agents
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"S1M" 1.0 0 -10899396 true "" "plot count turtles with [item 0 pure-local-strategy = TRUE]"
"S1U" 1.0 0 -2674135 true "" "plot count turtles with [item 1 pure-local-strategy = TRUE]"
"S2U" 1.0 0 -1264960 true "" "plot count turtles with [item 2 pure-local-strategy = TRUE]"
"S2M" 1.0 0 -8630108 true "" "plot count turtles with [item 3 pure-local-strategy = TRUE]"
"S3+" 1.0 0 -13791810 true "" "plot count turtles with [item 4 pure-local-strategy = TRUE]"
"S3-" 1.0 0 -5987164 true "" "plot count turtles with [item 5 pure-local-strategy = TRUE]"

PLOT
998
617
1267
789
Pure H-Strategies
time
agents
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"H-AAA" 1.0 0 -16777216 true "" "plot count turtles with [item 8 pure-strategy = TRUE]"
"H-AAB" 1.0 0 -6459832 true "" "plot count turtles with [item 9 pure-strategy = TRUE]"
"H-ABA" 1.0 0 -8630108 true "" "plot count turtles with [item 10 pure-strategy = TRUE]"
"H-ABB" 1.0 0 -2674135 true "" "plot count turtles with [item 11 pure-strategy = TRUE]"
"H-BAA" 1.0 0 -14835848 true "" "plot count turtles with [item 12 pure-strategy = TRUE]"
"H-BAB" 1.0 0 -13840069 true "" "plot count turtles with [item 13 pure-strategy = TRUE]"
"H-BBA" 1.0 0 -13791810 true "" "plot count turtles with [item 14 pure-strategy = TRUE]"
"H-BBB" 1.0 0 -4539718 true "" "plot count turtles with [item 15 pure-strategy = TRUE]"

PLOT
723
320
996
455
S-Moves
time
% of all moves
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"S1A" 1.0 1 -10899396 true "" "plot 100 * ((item 0 moves + item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves) / max list (1 (item 0 moves + item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves)))"
"S1B" 1.0 1 -2674135 true "" "plot 100 * ((item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves) / (item 0 moves + item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves))"
"S2A" 1.0 1 -1264960 true "" "plot 100 * ((item 2 moves + item 3 moves + item 4 moves + item 5 moves) / (item 0 moves + item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves))"
"S2B" 1.0 1 -8630108 true "" "plot 100 * ((item 3 moves + item 4 moves + item 5 moves) / (item 0 moves + item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves))"
"S3A" 1.0 1 -13791810 true "" "plot 100 * ((item 4 moves + item 5 moves) / (item 0 moves + item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves))"
"S3B" 1.0 1 -4539718 true "" "plot 100 * ((item 5 moves) / (item 0 moves + item 1 moves + item 2 moves + item 3 moves + item 4 moves + item 5 moves))"

PLOT
1000
320
1283
455
H-Moves
time
moves
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"H1A" 1.0 1 -2674135 true "" "plot item 6 moves + item 7 moves + item 8 moves + item 9 moves + item 10 moves + item 11 moves "
"H1B" 1.0 1 -1604481 true "" "plot item 7 moves + item 8 moves + item 9 moves + item 10 moves + item 11 moves "
"H2A" 1.0 1 -10899396 true "" "plot item 8 moves + item 9 moves + item 10 moves + item 11 moves "
"H2B" 1.0 1 -8330359 true "" "plot item 9 moves + item 10 moves + item 11 moves "
"H3A" 1.0 1 -13345367 true "" "plot item 10 moves + item 11 moves "
"H3B" 1.0 1 -8020277 true "" "plot item 11 moves "

PLOT
734
614
992
786
Pure S-Strategies
time
agents
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"S-MU+" 1.0 0 -7171555 true "" "plot count turtles with [item 0 pure-strategy = TRUE]"
"S-MM+" 1.0 0 -11221820 true "" "plot count turtles with [item 2 pure-strategy = TRUE]"
"S-UU+" 1.0 0 -2674135 true "" "plot count turtles with [item 4 pure-strategy = TRUE]"
"S-UM+" 1.0 0 -8630108 true "" "plot count turtles with [item 6 pure-strategy = TRUE]"
"S-MU-" 1.0 0 -723837 true "" "plot count turtles with [item 1 pure-strategy = TRUE]"
"S-MM-" 1.0 0 -4528153 true "" "plot count turtles with [item 3 pure-strategy = TRUE]"
"S-UM-" 1.0 0 -5204280 true "" "plot count turtles with [item 7 pure-strategy = TRUE]"
"S-UU-" 1.0 0 -1264960 true "" "plot count turtles with [item 5 pure-strategy = TRUE]"

PLOT
722
157
1281
317
Marking of information source
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"first hand information - unmarked" 1.0 0 -2674135 true "" "plot count turtles with [item 1 pure-local-strategy = TRUE]"
"first hand information - marked" 1.0 0 -10899396 true "" "plot count turtles with [item 0 pure-local-strategy = TRUE]"
"hearsay information - unmarked" 1.0 0 -2064490 true "" "plot count turtles with [item 12 pure-local-strategy = TRUE]"
"hearsay information - marked" 1.0 0 -13345367 true "" "plot count turtles with [item 13 pure-local-strategy = TRUE]"
"hearsay information - not shared further" 1.0 0 -7500403 true "" "plot count turtles with [item 14 pure-local-strategy = TRUE]"

MONITOR
137
66
215
111
people
count turtles
0
1
11

SWITCH
14
466
156
499
black-magic
black-magic
1
1
-1000

SWITCH
12
413
186
446
show-reputation
show-reputation
0
1
-1000

SLIDER
11
176
217
209
reputation-bet-marked
reputation-bet-marked
0
100
0.0
10
1
NIL
HORIZONTAL

SLIDER
11
216
217
249
reputation-bet-unmarked
reputation-bet-unmarked
0
100
0.0
10
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

## HOW IT WORKS

## HOW TO USE IT

## THINGS TO NOTICE


## THINGS TO TRY



## EXTENDING THE MODEL


## NETLOGO FEATURES


## RELATED MODELS

Preferential Attachment
Signaling Game

## CREDITS AND REFERENCES

TO DO!
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Speak Well Or Die" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = number-witnesses</exitCondition>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <exitCondition>count turtles = count witnesses</exitCondition>
    <metric>count turtles</metric>
    <metric>count turtles with [item 0 pure-local-strategy = TRUE]</metric>
    <metric>count turtles with [item 1 pure-local-strategy = TRUE]</metric>
    <metric>count turtles with [item 2 pure-local-strategy = TRUE]</metric>
    <metric>count turtles with [item 3 pure-local-strategy = TRUE]</metric>
    <metric>count turtles with [item 4 pure-local-strategy = TRUE]</metric>
    <metric>count turtles with [item 5 pure-local-strategy = TRUE]</metric>
    <enumeratedValueSet variable="black-magic">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-witnesses">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
      <value value="25"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reliability">
      <value value="0.5"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="marking-firsthand">
      <value value="1"/>
      <value value="50"/>
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="marking-hearsay">
      <value value="1"/>
      <value value="50"/>
      <value value="99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sharing-doubted">
      <value value="1"/>
      <value value="50"/>
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1000" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

dotted
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

small arrow
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 165 210
Line -7500403 true 150 150 135 210

small arrow dotted
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 165 210
Line -7500403 true 150 150 135 210
@#$#@#$#@
0
@#$#@#$#@
