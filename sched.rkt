(require minikanren)
(define (appendo l s out)
  (conde
    [(== l '()) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))])) 
(define (caro p
a)
(fresh (d)
(== (cons a
d) p)))
(define (cdro p d)
(fresh (a)
(== (cons a d) p)))
(define (membero x l)
(conde
((caro l x))
((fresh (d)
(cdro l d)
(membero x
d)))))
(define succeed (== #f #f))

(define (init_sched s) ; - расписание одной группы на неделю
  (fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9 b1 b2 b3 b4 b5 b6 b7 b8 c1 c2 c3 c4) (== s `((,a1,a2,a3,a4)(,a5,a6,a7,a8)(,b1,b2,b3,b4)(,b5,b6,b7,b8)(,c1,c2,c3,c4)))))

(define (init_manysched a1 a2 a3 a4 a5 a6 a7 a8); - расписание 8-ми групп/преподавалетелй/аудиторий на неделю
  (fresh (b1 b2 b3 b4 b5 b6 b7 b8) (init_sched b1) (init_sched b2) (init_sched b3) (init_sched b4) (init_sched b5) (init_sched b6) (init_sched b7) (init_sched b8) (== a1 b1) (== a2 b2) (== a3 b3) (== a4 b4) (== a5 b5) (== a6 b6) (== a7 b7) (== a8 b8)))

(define (ins1 subj schedgroup teachersched schedclass namegroup teachername classnumber); - вставка предмета в определенный день на первую пару, следующие 3 реляции подобны
  (conde
   [(fresh (a1 a2 a3 b1 b2 b3 c1 c2 c3)
           (== schedgroup `(,a1 . ,a2)) (== a3 `(,subj ,teachername ,classnumber)) (== a1 a3)
           (== teachersched `(,b1 . ,b2)) (== b3 `(,subj,namegroup,classnumber)) (== b1 b3)
           (== schedclass `(,c1 . ,c2)) (== c3 `(,subj,teachername,namegroup)) (== c1 c3)
           )]))

(define (ins2 subj schedgroup teachersched schedclass namegroup teachername classnumber)
  (conde
   [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4)
           (== schedgroup `(,a1,a2 . ,a3)) (== a4 `(,subj ,teachername ,classnumber)) (== a2 a4)
           (== teachersched `(,b1,b2 . ,b3)) (== b4 `(,subj,namegroup,classnumber)) (== b2 b4)
           (== schedclass `(,c1,c2 . ,c3)) (== c4 `(,subj ,teachername ,classnumber)) (== c2 c4)
           )]))

(define (ins3 subj schedgroup teachersched schedclass namegroup teachername classnumber)
  (conde
   [(fresh (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5)
           (== schedgroup `(,a1,a2,a3,a4)) (== a5 `(,subj ,teachername ,classnumber)) (== a3 a5)
           (== teachersched `(,b1,b2,b3,b4)) (== b5 `(,subj,namegroup,classnumber)) (== b3 b5)
           (== schedclass `(,c1,c2,c3,c4)) (== c5 `(,subj ,teachername ,classnumber)) (== c3 c5)
           )]))


(define (ins4 subj schedgroup teachersched schedclass namegroup teachername classnumber)
  (conde
   [(fresh (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5)
           (== schedgroup `(,a1,a2,a3,a4)) (== a5 `(,subj ,teachername ,classnumber)) (== a4 a5)
           (== teachersched `(,b1,b2,b3,b4)) (== b5 `(,subj,namegroup,classnumber)) (== b4 b5)
           (== schedclass `(,c1,c2,c3,c4)) (== c5 `(,subj ,teachername ,classnumber)) (== c4 c5)
           )]))

(define (insall subj schedgroup teachersched schedclass namegroup teachername classnumber); здесь происходит вставка предмета
  (fresh (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5)
         (== schedgroup `(,a1,a2,a3,a4,a5)) (== teachersched `(,b1,b2,b3,b4,b5)) (== schedclass `(,c1,c2,c3,c4,c5)) 
         (conde
          [(ins2 subj a1 b1 c1 namegroup teachername classnumber)]
          [(ins2 subj a2 b2 c2 namegroup teachername classnumber)]
          [(ins2 subj a3 b3 c3 namegroup teachername classnumber)]
          [(ins2 subj a4 b4 c4 namegroup teachername classnumber)]
          [(ins2 subj a5 b5 c5 namegroup teachername classnumber)]
          [(ins3 subj a1 b1 c1 namegroup teachername classnumber)]
          [(ins3 subj a2 b2 c2 namegroup teachername classnumber)]
          [(ins3 subj a3 b3 c3 namegroup teachername classnumber)]
          [(ins3 subj a4 b4 c4 namegroup teachername classnumber)]
          [(ins3 subj a5 b5 c5 namegroup teachername classnumber)]
          [(ins4 subj a1 b1 c1 namegroup teachername classnumber)]
          [(ins4 subj a2 b2 c2 namegroup teachername classnumber)]
          [(ins4 subj a3 b3 c3 namegroup teachername classnumber)]
          [(ins4 subj a4 b4 c4 namegroup teachername classnumber)]
          [(ins4 subj a5 b5 c5 namegroup teachername classnumber)]         
          [(ins1 subj a1 b1 c1 namegroup teachername classnumber)]
          [(ins1 subj a2 b2 c2 namegroup teachername classnumber)]
          [(ins1 subj a3 b3 c3 namegroup teachername classnumber)]
          [(ins1 subj a4 b4 c4 namegroup teachername classnumber)]
          [(ins1 subj a5 b5 c5 namegroup teachername classnumber)]
          )))

(define (timetableonegroup studyplan schedgroup teachersched schedclass classessubj namegroup allteachername allclassnumber)
  (conde
   [(== studyplan '()) succeed]
   [(fresh (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5) (caro studyplan a1) (caro classessubj a3) (membero a1 a3) (caro teachersched a2)  (caro schedclass a5) (caro allteachername b3) (caro allclassnumber b4) (insall a1 schedgroup a2 a5 namegroup b3 b4)
          (cdro studyplan b1) (cdro teachersched b2) (cdro allteachername b5) (timetableonegroup b1 schedgroup b2 schedclass classessubj namegroup b5 allclassnumber))
           ]
   [(fresh (a1 a2 a3 a4 a5 a6) (cdro classessubj a1) (cdro schedclass a2) (cdro allclassnumber a3) (timetableonegroup studyplan schedgroup teachersched a2 a1 namegroup allteachername a3))]
   ))

(define (sched studyplanallgroup schedallgroup allteachersched schedclass classessubj allnamegroup allteachername allclassnumber) ;составляет расписание на все группы, но только практики
  (conde
   [(== studyplanallgroup '()) succeed]
   [(fresh (a1 a2 a3 namegroup schedone a4 a5) (caro studyplanallgroup a1) (caro schedallgroup schedone) (caro allnamegroup namegroup) (caro allteachersched a3) (caro allteachername a5) 
           (timetableonegroup a1 schedone a3 schedclass classessubj namegroup a5 allclassnumber))
    (fresh (a4 a5 a6 b1 b2 b3) (cdro studyplanallgroup a4) (cdro schedallgroup a5) (cdro allteachersched a6) (cdro allnamegroup b1) (cdro allteachername b2)
           (sched a4 a5 a6 schedclass classessubj b1 b2 allclassnumber))
           ]
   ))

(define (inslecture1 subj first second third fourth teachersched schedclass potokname teachername classnumber)
   (fresh (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7 c1 c2 c3 c4 c5 c6 c7 x1 x2 x3 x4 x5 x6 x7 n1 n2 n3 n4 n5 n6 n7 m1 m2 m3 m4 m5 m6 m7 forstudent forteacher)
         (== teachersched `(,a1,a2,a3,a4))
         (== first `(,b1,b2,b3,b4))
         (== second `(,c1,c2,c3,c4))
         (== third `(,x1,x2,x3,x4))
         (== fourth `(,n1,n2,n3,n4))
         (== schedclass `(,m1,m2,m3,m4))
         (== forstudent `(,subj,teachername,classnumber))
          (== forteacher `(,subj,classnumber,potokname))
         (conde
          [(== a1 forteacher)
            (== b1 forstudent)
            (== c1 forstudent)
            (== x1 forstudent)
            (== n1 forstudent)
            (== m1 forstudent)
            ]
          )))

(define (inslecture2 subj first second third fourth teachersched schedclass potokname teachername classnumber)
   (fresh (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7 c1 c2 c3 c4 c5 c6 c7 x1 x2 x3 x4 x5 x6 x7 n1 n2 n3 n4 n5 n6 n7 m1 m2 m3 m4 m5 m6 m7 forstudent forteacher)
         (== teachersched `(,a1,a2,a3,a4))
         (== first `(,b1,b2,b3,b4))
         (== second `(,c1,c2,c3,c4))
         (== third `(,x1,x2,x3,x4))
         (== fourth `(,n1,n2,n3,n4))
         (== schedclass `(,m1,m2,m3,m4))
         (== forstudent `(,subj,teachername,classnumber))
          (== forteacher `(,subj,classnumber,potokname))
         (conde
          [(== a2 forteacher)
            (== b2 forstudent)
            (== c2 forstudent)
            (== x2 forstudent)
            (== n2 forstudent)
            (== m2 forstudent)
            ]
          )))

(define (inslecture3 subj first second third fourth teachersched schedclass potokname teachername classnumber)
   (fresh (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7 c1 c2 c3 c4 c5 c6 c7 x1 x2 x3 x4 x5 x6 x7 n1 n2 n3 n4 n5 n6 n7 m1 m2 m3 m4 m5 m6 m7 forstudent forteacher)
         (== teachersched `(,a1,a2,a3,a4))
         (== first `(,b1,b2,b3,b4))
         (== second `(,c1,c2,c3,c4))
         (== third `(,x1,x2,x3,x4))
         (== fourth `(,n1,n2,n3,n4))
         (== schedclass `(,m1,m2,m3,m4))
         (== forstudent `(,subj,teachername,classnumber))
         (== forteacher `(,subj,classnumber,potokname))
         (conde
          [(== a3 forteacher)
            (== b3 forstudent)
            (== c3 forstudent)
            (== x3 forstudent)
            (== n3 forstudent)
            (== m3 forstudent)
            ]
          )))

(define (inslecture4 subj first second third fourth teachersched schedclass potokname teachername classnumber)
   (fresh (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7 c1 c2 c3 c4 c5 c6 c7 x1 x2 x3 x4 x5 x6 x7 n1 n2 n3 n4 n5 n6 n7 m1 m2 m3 m4 m5 m6 m7 forstudent forteacher)
         (== teachersched `(,a1,a2,a3,a4))
         (== first `(,b1,b2,b3,b4))
         (== second `(,c1,c2,c3,c4))
         (== third `(,x1,x2,x3,x4))
         (== fourth `(,n1,n2,n3,n4))
         (== schedclass `(,m1,m2,m3,m4))
         (== forstudent `(,subj,teachername,classnumber))
          (== forteacher `(,subj,classnumber,potokname))
         (conde
          [(== a4 forteacher)
            (== b4 forstudent)
            (== c4 forstudent)
            (== x4 forstudent)
            (== n4 forstudent)
            (== m4 forstudent)
            ]
          )))


(define (inslecall subj teachersched first second third fourth schedclass potokname teachername classnumber)
  (fresh (a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7 c1 c2 c3 c4 c5 c6 c7 x1 x2 x3 x4 x5 x6 x7 n1 n2 n3 n4 n5 n6 n7 m1 m2 m3 m4 m5 m6 m7)
          (== teachersched `(,a1,a2,a3,a4,a5))
         (== first `(,b1,b2,b3,b4,b5))
         (== second `(,c1,c2,c3,c4,c5))
         (== third `(,x1,x2,x3,x4,x5))
         (== fourth `(,n1,n2,n3,n4,n5))
         (== schedclass `(,m1,m2,m3,m4,m5))
         (conde
          [(inslecture1 subj b1 c1 x1 n1 a1 m1 potokname teachername classnumber)]
          [(inslecture2 subj b1 c1 x1 n1 a1 m1 potokname teachername classnumber)]
          [(inslecture3 subj b1 c1 x1 n1 a1 m1 potokname teachername classnumber)]
          [(inslecture4 subj b1 c1 x1 n1 a1 m1 potokname teachername classnumber)]
          [(inslecture1 subj b2 c2 x2 n2 a2 m2 potokname teachername classnumber)]
          [(inslecture2 subj b2 c2 x2 n2 a2 m2 potokname teachername classnumber)]
          [(inslecture3 subj b2 c2 x2 n2 a2 m2 potokname teachername classnumber)]
          [(inslecture4 subj b2 c2 x2 n2 a2 m2 potokname teachername classnumber)]
          [(inslecture1 subj b3 c3 x3 n3 a3 m3 potokname teachername classnumber)]
          [(inslecture2 subj b3 c3 x3 n3 a3 m3 potokname teachername classnumber)]
          [(inslecture3 subj b3 c3 x3 n3 a3 m3 potokname teachername classnumber)]
          [(inslecture4 subj b3 c3 x3 n3 a3 m3 potokname teachername classnumber)]
          [(inslecture1 subj b4 c4 x4 n4 a4 m4 potokname teachername classnumber)]
          [(inslecture2 subj b4 c4 x4 n4 a4 m4 potokname teachername classnumber)]
          [(inslecture3 subj b4 c4 x4 n4 a4 m4 potokname teachername classnumber)]
          [(inslecture4 subj b4 c4 x4 n4 a4 m4 potokname teachername classnumber)]
          [(inslecture1 subj b5 c5 x5 n5 a5 m5 potokname teachername classnumber)]
          [(inslecture2 subj b5 c5 x5 n5 a5 m5 potokname teachername classnumber)]
          [(inslecture3 subj b5 c5 x5 n5 a5 m5 potokname teachername classnumber)]
          [(inslecture4 subj b5 c5 x5 n5 a5 m5 potokname teachername classnumber)]
          )))

(define (lec studyplanlec first second third fourth allteachersched schedclass classessubj potokname allteachername allclassnumber)
  (conde
   [(== studyplanlec '()) succeed]
   [(fresh (a1 a2 a3 a4  teachername classnumber) (caro studyplanlec a1) (caro allteachersched a2) (caro classessubj a3) (membero a1 a3) (caro schedclass a4)
           (caro allteachername teachername) (caro allclassnumber classnumber) (inslecall a1 a2 first second third fourth a4 potokname teachername classnumber))
    (fresh(b1 b2 b3) (cdro studyplanlec b1) (cdro allteachersched b2) (cdro allteachername b3)  (lec b1 first second third fourth b2 schedclass classessubj potokname b3 allclassnumber))]
    
    [(fresh (a1 a2 a3) (cdro classessubj a1) (cdro schedclass a2) (cdro allclassnumber a3) (lec studyplanlec first second third fourth allteachersched a2 a1 potokname allteachername a3))]
   ))


(time (run 1 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 b07 b08 b09 b10 teachersched schedclass n1 n2 n3 m1 m2 x1 x2 x3 x4 x5 c07 c08 c09 c10 l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 l11 l12 l13 l14) (init_manysched c07 c08 c09 c10 l1 l2 l3 l4) (init_sched b5) (init_sched b6) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched b1 b2 b3 b4 b07 b08 b09 b10); работает за 1.5 секунды (1 вариант), 10 вариантов за 2 секунды, 100 за 4 секунды / со вставкой лекций в среду работает также за 1,5 секунды для 1 раза, но для 100 раз уже 3,99 секунды
           (== teachersched `(,a1,a2,a3,a4,a5,a6)) (== schedclass `(,b1,b2,b3,b4))
        
         (lec '(matanlec1 matanlec2 alglec geomlec inflec difflec) b07 b08 b09 b10 `(,a8,a8,a2,a3,a4,a4) `(,b1,b2,b3,b4) '((matanlec1 matanlec2 alglec geomlec inflec difflec)(matanlec1 matanlec2 alglec geomlec inflec difflec)(matanlec1 matanlec2 alglec geomlec inflec difflec)(matanlec1 matanlec2 alglec geomlec inflec difflec))
              'matobes1 '(Иванов Иванов Петров Васечкин 4препод 4препод) '(1аудитория 2аудитория 3аудитория 4аудитория))
         (lec '(matanlec1 matanlec2 alglec geomlec inflec difflec) c07 c08 c09 c10 `(,a8,a8,a2,a3,a4,a4) `(,b1,b2,b4,b3) '((matanlec1 matanlec2 alglec geomlec inflec difflec)(matanlec1 matanlec2 alglec geomlec inflec difflec)(matanlec1 matanlec2 alglec geomlec inflec difflec)(matanlec1 matanlec2 alglec geomlec inflec difflec))
              'matobes2 '(Иванов Иванов Петров Васечкин 4препод 4препод) '(1аудитория 2аудитория 4аудитория 3аудитория))
           (sched '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1)  (matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1))
                  `(,b07,b08,b09,b10) `((,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)) `(,b1,b2,b3,b4)
                  '((matan1 matan2 alg1 geom1 diff1 prog1 eng1 matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture)(matan1 matan2 alg1 geom1 diff1 prog1 eng1)(matan1 matan2 alg1 geom1 diff1 prog1 eng1)(matan1 matan2 alg1 geom1 diff1 prog1 eng1))
                  '(группа1 группа2 группа3 группа4) '((преп1 преп1 преп2 преп3 преп4 преп5 преп6)(преп1 преп1 преп2 преп3 преп4 преп5 преп6)(преп1 преп1 преп2 преп3 преп4 преп5 преп6)(преп1 преп1 преп2 преп3 преп4 преп5 преп6))
                  '(1аудитория 2аудитория 3аудитория 4аудитория)
                 )
           (sched '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1)  (matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1))
                  `(,c07,c08,c09,c10) `((,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)) `(,b1,b2,b3,b4)
                  '((matan1 matan2 alg1 geom1 diff1 prog1 eng1 matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture)(matan1 matan2 alg1 geom1 diff1 prog1 eng1)(matan1 matan2 alg1 geom1 diff1 prog1 eng1)(matan1 matan2 alg1 geom1 diff1 prog1 eng1))
                  '(группа5 группа6 группа7 группа8) '((преп1 преп1 преп2 преп3 преп4 преп5 преп6)(преп1 преп1 преп2 преп3 преп4 преп5 преп6)(преп1 преп1 преп2 преп3 преп4 преп5 преп6)(преп1 преп1 преп2 преп3 преп4 преп5 преп6))
                  '(1аудитория 2аудитория 3аудитория 4аудитория)
                 )
           
        
          
           (== q `(,b07,b08,b09,b10,c07,c08,c09,c10,a8,a2,a3,a4,a1,a5,a6,a7,b5,b1,b2,b3,b4,b6)))))
