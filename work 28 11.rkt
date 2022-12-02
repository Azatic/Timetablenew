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
(define (nullo x)
(== '() x))
(define succeed (== #f #f))
(define (init_sched s) ; - расписание группы на неделю
  (fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9 b1 b2 b3 b4 b5 b6 b7 b8 b9 c1 c2 c3 c4 c5 c6) (== s `((,a1,a2,a3,a4)(,a5,a6,a7,a8)(,b1,b2,b3,b4)(,b5,b6,b7,b8)(,c1,c2,c3,c4)))))

(define (init_manysched a1 a2 a3 a4 a5 a6 a7 a8)
  (fresh (b1 b2 b3 b4 b5 b6 b7 b8) (init_sched b1) (init_sched b2) (init_sched b3) (init_sched b4) (init_sched b5) (init_sched b6) (init_sched b7) (init_sched b8) (== a1 b1) (== a2 b2) (== a3 b3) (== a4 b4) (== a5 b5) (== a6 b6) (== a7 b7) (== a8 b8)))

;как выглядят основные объекты: schedallgroup
;teachersched
;schedclass
;можно как написать, беру каку. то функцию на самой схеме, одним из конъюнктов которой будет сама проверка на возможность вставить нужную пару в нужное место
;(define (insertfri1 subjteacher teachersched subj schedonegroup classes schedclass)

(define (ins1 subj schedgroup teachersched schedclass namegroup) ;в функцию передается расписание уже на определенный день, после чего на первую пару вставляется предмет
  (conde
   [(fresh (a1 b1 c1 b9)
           (== b9 `(,subj ,namegroup))
     (caro schedgroup a1) (== a1 subj)
     (caro teachersched b1) (== b1 b9)
     (caro schedclass c1) (== c1 `(,subj ,namegroup))
           )]
   ))

(define (ins2 subj schedgroup teachersched schedclass namegroup) 
  (conde
   [(fresh (a1 b1 c1 a2 b2 c2 b3)
           (== b3 `(,subj ,namegroup))
     (cdro schedgroup a1) (caro a1 a2) (== a2 subj)
     (cdro teachersched b1) (caro b1 b2) (== b2 b3)
     (cdro schedclass c1) (caro c1 c2) (== c2 `(,subj ,namegroup))
           )]
   ))
(define (ins3 subj schedgroup teachersched schedclass namegroup) 
  (conde
   [(fresh (a1 b1 c1 a2 b2 c2 a3 b3 c3 b9)
           (== b9 `(,subj ,namegroup))
     (cdro schedgroup a1) (cdro a1 a2) (caro a2 a3) (== a3 subj)
     (cdro teachersched b1) (cdro b1 b2) (caro b2 b3) (== b3 b9)
     (cdro schedclass c1) (cdro c1 c2) (caro c2 c3) (== c3 `(,subj ,namegroup))
           )]
   ))
(define (ins4 subj schedgroup teachersched schedclass namegroup) 
  (conde
   [(fresh (a1 b1 c1 a2 b2 c2 a3 b3 c3 a4 b4 c4 b9)
           (== b9 `(,subj ,namegroup))
 (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (caro a3 a4) (== a4 subj)
     (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (caro b3 b4) (== b4 b9)
     (cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (caro c3 c4) (== c4 `(,subj ,namegroup))
           )]
   ))


(define (insmonday subj schedgroup teachersched schedclass namegroup) ; пытается вставить предмет в понедельник в одну из пар
 (fresh (a1 a2 b1 b2 c1 c2)
    (caro schedgroup a1)
    (caro teachersched b1) 
    (caro schedclass c1)
    (conde
   [(ins1 subj a1 b1 c1 namegroup)]       
   [(ins2 subj a1 b1 c1 namegroup)]  
   [(ins3 subj a1 b1 c1 namegroup)] 
   [(ins4 subj a1 b1 c1 namegroup)]
   )))

(define (instuesday subj schedgroup teachersched schedclass namegroup)
  (fresh (a1 a2 b1 b2 c1 c2)
    (cdro schedgroup a1) (caro a1 a2) ;возвращает вторник в расписании группы
    (cdro teachersched b1) (caro b1 b2)
    (cdro schedclass c1) (caro c1 c2)
  (conde
   [(ins1 subj a2 b2 c2 namegroup)]
   [(ins2 subj a2 b2 c2 namegroup)]
   [(ins3 subj a2 b2 c2 namegroup)]   
   [(ins4 subj a2 b2 c2 namegroup)])))

(define (inswednesday subj schedgroup teachersched schedclass namegroup)
  (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3)
    (cdro schedgroup a1) (cdro a1 a2) (caro a2 a3) 
    (cdro teachersched b1) (cdro b1 b2) (caro b2 b3)
    (cdro schedclass c1) (cdro c1 c2) (caro c2 c3)
  (conde
   [(ins1 subj a3 b3 c3 namegroup)]
   [(ins2 subj a3 b3 c3 namegroup)]
   [(ins3 subj a3 b3 c3 namegroup)]   
   [(ins4 subj a3 b3 c3 namegroup)])))

(define (insthursday subj schedgroup teachersched schedclass namegroup)
  (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3 a4 b4 c4)
    (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (caro a3 a4) 
    (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (caro b3 b4)
    (cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (caro c3 c4)
  (conde
   [(ins1 subj a4 b4 c4 namegroup)]
   [(ins2 subj a4 b4 c4 namegroup)]
   [(ins3 subj a4 b4 c4 namegroup)]   
   [(ins4 subj a4 b4 c4 namegroup)])))

(define (insfriday subj schedgroup teachersched schedclass namegroup)
  (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3 a4 b4 c4 a5 b5 c5)
    (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (cdro a3 a4) (caro a4 a5)
    (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (cdro b3 b4) (caro b4 b5)
    (cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (cdro c3 c4) (caro c4 c5)
  (conde
   [(ins1 subj a5 b5 c5 namegroup)]
   [(ins2 subj a5 b5 c5 namegroup)]
   [(ins3 subj a5 b5 c5 namegroup)]   
   [(ins4 subj a5 b5 c5 namegroup)])))

(define (insall subj schedgroup teachersched schedclass namegroup)
  (conde
   [(insmonday subj schedgroup teachersched schedclass namegroup)]
   [(instuesday subj schedgroup teachersched schedclass namegroup)]
   [(inswednesday subj schedgroup teachersched schedclass namegroup)]
   [(insthursday subj schedgroup teachersched schedclass namegroup)]
   [(insfriday subj schedgroup teachersched schedclass namegroup)]
   ))

;(define (insall2 subj schedgroup teachersched schedclass)
 ; (fresh (a1 a2 b1 b2 c1 c2 a3 b3 c3 a4 b4 c4 a5 b5 c5)
  ;  (cdro schedgroup a1) (cdro a1 a2) (cdro a2 a3) (cdro a3 a4) (caro a4 a5)
   ; (cdro teachersched b1) (cdro b1 b2) (cdro b2 b3) (cdro b3 b4) (caro b4 b5)
    ;(cdro schedclass c1) (cdro c1 c2) (cdro c2 c3) (cdro c3 c4) (caro c4 c5)
 ; (conde
   
   
  
;(define (timetableonegroup studyplan schedgroup teachersched schedclass classessubj);здесь происходит пробег по аудиториям
 ; (conde
  ; [(== studyplan '()) succeed]
   ;[(conde
    ; [(fresh(a1 a2 a3 a4 a5) (caro studyplan a1) (caro teachersched a3) (caro classessubj a4) (membero a1 a4) (caro schedclass a5) (insall a1 schedgroup a3 a5))]
     ;[(fresh (b1 b2 b3) (cdro classessubj b1) (cdro schedclass b2) (timetableonegroup studyplan schedgroup teachersched b2 b1))])
      ;   (fresh(a2 a4) (cdro studyplan a2) (cdro teachersched a4) (timetableonegroup a2 schedgroup a4 schedclass classessubj))
    ;]
   ;))

(define (timetableonegroup2 studyplan schedgroup teachersched schedclass classessubj namegroup)
  (conde
   [(== studyplan '()) succeed]
   [(fresh (a1 a2 a3 a4 a5 b1 b2) (caro studyplan a1) (caro teachersched a2) (caro classessubj a3) (membero a1 a3) (caro schedclass a5) (insall a1 schedgroup a2 a5 namegroup)
          (cdro studyplan b1) (cdro teachersched b2) (timetableonegroup2 b1 schedgroup b2 schedclass classessubj namegroup))
           ]
   [(fresh (a1 a2 a3 a4 a5 a6) (cdro classessubj a1) (cdro schedclass a2) (timetableonegroup2 studyplan schedgroup teachersched a2 a1 namegroup))]
   ))

(define (sched studyplanallgroup schedallgroup allteachersched schedclass classessubj) ;составляет расписание на все группы, но только практики
  (conde
   [(== studyplanallgroup '()) succeed]
   [(fresh (a1 a2 a3 namegroup schedone) (caro studyplanallgroup a1) (caro schedallgroup a2) (caro a2 schedone) (cdro a2 namegroup) (caro allteachersched a3) (timetableonegroup2 a1 schedone a3 schedclass classessubj namegroup))
    (fresh (a4 a5 a6) (cdro studyplanallgroup a4) (cdro schedallgroup a5) (cdro allteachersched a6) (sched     a4 a5 a6 schedclass classessubj))
           ]
   ))

(define (inslectire1 subj teachersched first second third fourth schedclass)
  (conde
   [(caro teachersched subj)
    (caro first subj)
    (caro second subj)
    (caro third subj)
    (caro fourth subj)
    (caro schedclass subj)]
   ))

(define (inslectire2 subj teachersched first second third fourth schedclass)
(fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9)
  (conde
   [(cdro teachersched a1) (caro a1 subj)
    (cdro first a2) (caro a2 subj)
    (cdro second a3) (caro a3 subj)
    (cdro third a4) (caro a4 subj)
    (cdro fourth a5) (caro a5 subj)
    (cdro schedclass a6) (caro a6 subj)
    ] 
   )))

(define (inslectire3 subj teachersched first second third fourth schedclass)
(fresh (a1 a2 a3 a4 a5 a6 b6 b1 b2 b3 b4 b5)
  (conde
   [(cdro teachersched a1) (cdro a1 b1) (caro b1 subj)
    (cdro first a2) (cdro a2 b2) (caro b2 subj)
    (cdro second a3) (cdro a3 b3)(caro b3 subj)
    (cdro third a4) (cdro a4 b4)(caro b4 subj)
    (cdro fourth a5) (cdro a5 b5)(caro b5 subj)
    (cdro schedclass a6) (cdro a6 b6) (caro b6 subj)
    ] 
   )))

(define (inslectire4 subj teachersched first second third fourth schedclass)
(fresh (a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 c1 c2 c3 c4 c5 a6 b6 c6)
  (conde
   [(cdro teachersched a1) (cdro a1 b1) (cdro b1 c1) (caro c1 subj)
    (cdro first a2) (cdro a2 b2) (cdro b2 c2) (caro c2 subj)
    (cdro second a3) (cdro a3 b3)(cdro b3 c3) (caro c3 subj)
    (cdro third a4) (cdro a4 b4)(cdro b4 c4) (caro c4 subj)
    (cdro fourth a5) (cdro a5 b5)(cdro b5 c5) (caro c5 subj)
    (cdro schedclass a6) (cdro a6 b6) (cdro b6 c6) (caro c6 b6)
    ] 
   )))

(define (inslecmonday subj teachersched first second third fourth schedclass)
  (fresh (a1 a2 a3 a4 a5 a6)
         (caro teachersched a1)
         (caro first a2)
         (caro second a3)
         (caro third a4)
         (caro fourth a5)
         (caro schedclass a6)
         (conde
          [(inslectire1 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire2 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire3 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire4 subj a1 a2 a3 a4 a5 a6)]
          )))

(define (inslectuesday subj teachersched first second third fourth schedclass)
  (fresh (a1 a2 a3 a4 a5 a6 b1 b2 b3 b4 b5 b6)
         (cdro teachersched b1) (caro b1 a1)
         (cdro first b2) (caro b2 a2)
         (cdro second b3) (caro b3 a3)
         (cdro third b4) (caro b4 a4)
         (cdro fourth b5) (caro b5 a5)
         (cdro schedclass b6) (caro b6 a6)
         (conde
          [(inslectire1 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire2 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire3 subj a1 a2 a3 a4 a5 a6)]
          [(inslectire4 subj a1 a2 a3 a4 a5 a6)]
          )))


;(==  b07 `(,n1 ,n2 ('chill . ,m1) . ,m2)) 
(define (inslecwednesday subj teachersched first second third fourth schedclass)
  (fresh (a1 a2 a3 a4 z1 z2 z3 z4 x1 x2 x3 x4 c1 c2 c3 c4 v1 v2 v3 v4 b1 b2 b3 b4)
         (== teachersched `(,a1,a2,a3 . ,a4))
         (== first `(,z1,z2,z3 . ,z4))
         (== second `(,x1,x2,x3 . ,x4))
         (== third `(,c1,c2,c3 . ,c4))
         (== fourth `(,v1,v2,v3 . ,v4))
         (== schedclass `(,b1,b2,b3 . ,b4))
         (conde
          [(inslectire1 subj a3 z3 x3 c3 v3 b3)]
          [(inslectire2 subj a3 z3 x3 c3 v3 b3)]
          [(inslectire3 subj a3 z3 x3 c3 v3 b3)]
          [(inslectire4 subj a3 z3 x3 c3 v3 b3)]
          )))

(define (inslecthursday subj teachersched first second third fourth schedclass)
  (fresh (a1 a2 a3 a4 z1 z2 z3 z4 x1 x2 x3 x4 c1 c2 c3 c4 v1 v2 v3 v4 b1 b2 b3 b4 a5 z5 x5 c5 v5 b5)
         (== teachersched `(,a1,a2,a3,a4,a5))
         (== first `(,z1,z2,z3,z4,z5))
         (== second `(,x1,x2,x3,x4,x5))
         (== third `(,c1,c2,c3,c4,c5))
         (== fourth `(,v1,v2,v3,v4,v5))
         (== schedclass `(,b1,b2,b3,b4,v5))
         (conde
          [(inslectire1 subj a4 z4 x4 c4 v4 b4)]
          [(inslectire2 subj a4 z4 x4 c4 v4 b4)]
          [(inslectire3 subj a4 z4 x4 c4 v4 b4)]
          [(inslectire4 subj a4 z4 x4 c4 v4 b4)]         
          )))

(define (inslecfriday subj teachersched first second third fourth schedclass)
  (fresh (a1 a2 a3 a4 z1 z2 z3 z4 x1 x2 x3 x4 c1 c2 c3 c4 v1 v2 v3 v4 b1 b2 b3 b4 a5 z5 x5 c5 v5 b5)
         (== teachersched `(,a1,a2,a3,a4,a5))
         (== first `(,z1,z2,z3,z4,z5))
         (== second `(,x1,x2,x3,x4,x5))
         (== third `(,c1,c2,c3,c4,c5))
         (== fourth `(,v1,v2,v3,v4,v5))
         (== schedclass `(,b1,b2,b3,b4,v5))
         (conde
          [(inslectire1 subj a5 z5 x5 c5 v5 b5)]
          [(inslectire2 subj a5 z5 x5 c5 v5 b5)]
          [(inslectire3 subj a5 z5 x5 c5 v5 b5)]
          [(inslectire4 subj a5 z5 x5 c5 v5 b5)]         
          )))

  
(define (inslecall subj teachersched first second third fourth schedclass)
  (conde
   [(inslecmonday subj teachersched first second third fourth schedclass)]
   [(inslectuesday subj teachersched first second third fourth schedclass)]
   [(inslecwednesday subj teachersched first second third fourth schedclass)];без четверга делает за 3 секунды, с ним не делает и за секунд 20, почему
   [(inslecthursday subj teachersched first second third fourth schedclass)];с полной назрузкой за 4,5 секунды. Видимо связано с тем, что перебор неудобен в сторону четверга
   [(inslecfriday subj teachersched first second third fourth schedclass)]; пятницей без четверга тоже 4 секунды
   ))

(define (lec studyplanlec first second third fourth allteachersched schedclass classessubj) 
  (conde
   [(== studyplanlec '()) succeed]
   [(fresh (a1 a2 a3 a4) (caro studyplanlec a1) (caro allteachersched a2) (caro classessubj a3) (membero a1 a3) (caro schedclass a4) (inslecall a1 a2 first second third fourth a4))
    (fresh(b1 b2 b3) (cdro studyplanlec b1) (cdro allteachersched b2) (lec b1 first second third fourth b2 schedclass classessubj))]
    
    [(fresh (a1 a2 a3) (cdro classessubj a1) (cdro schedclass a2) (lec studyplanlec first second third fourth allteachersched a2 a1))]
   ))




  
;(time (run 10 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8)
 ;                        
  ;                     (sched '((matan geom alg eng)(matan2 eng2)) `(,a1,a4) `((,a2,a2,a2,a2)(,a2,a2)) `(,a3) '((matan alg geom eng matan2 geom2 alg2 eng2)))
   ;               (== q `(,a1,a4,a2,a3)))))

;(time (run 1 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 b07 b08 b09 b10 teachersched schedclass n1 n2 n3 m1 m2 ) (init_sched b5) (init_sched b6) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched b1 b2 b3 b4 b07 b08 b09 b10); за 662
;           (== teachersched `(,a1,a2,a3,a4,a5,a6)) (== schedclass `(,b1,b2,b3,b4))
           ;(caro b07 n1) (caro n1 n2) (== n2 '(chill)) (caro b08 m1) (caro m1 m2) (== m2 '(chill))
;           (cdro b07 n1) (cdro n1 n2) (caro n2 n3) (caro n3 m1) (== m1 '(chill))
;           ;(==  b07 `(,n1 ,n2 ('chill . ,m1) . ,m2)) 
           ;(lecturefor4group '(matanlec1 alglec geomlec inflec difflec matanlec2) '((matanlec1 alglec geomlec inflec difflec matanlec2 proglec)) `(,b5) b07 b08 b09 b10 '((matanlec1 alglec geomlec inflec difflec matanlec2 proglec)) `(,b6))
;           (sched '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan3 matan4 alg2 geom2 diff2 prog2 eng2)  (matan5 matan6 alg3 geom3 diff3 prog3 eng3) (matan7 matan8 alg4 geom4 diff4 prog4 eng4))
;                  `(,b07,b08,b09,b10) `((,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)) `(,b1,b2,b3,b4)
 ;                 '((matan1 matan2 alg1 geom1 diff1 prog1 eng1 matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture)(matan3 matan4 alg2 geom2 diff2 prog2 eng2)(matan5 matan6 alg3 geom3 diff3 prog3 eng3)(matan7 matan8 alg4 geom4 diff4 prog4 eng4)))
 ;         (== q `(,b07,b08,b09,b10,teachersched,b5,schedclass,b6)))))
;(lec studyplanlec first second third fourth allteachersched schedclass classessubj) 

;(time (run 1 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 b07 b08 b09 b10 teachersched schedclass n1 n2 n3 m1 m2 ) (init_sched b5) (init_sched b6) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched b1 b2 b3 b4 b07 b08 b09 b10); работает за 1.5 секунды (1 вариант), 10 вариантов за 2 секунды, 100 за 4 секунды / со вставкой лекций в среду работает также за 1,5 секунды для 1 раза, но для 100 раз уже 3,99 секунды
;           (== teachersched `(,a1,a2,a3,a4,a5,a6)) (== schedclass `(,b1,b2,b3,b4))
;           (lec '(matanlec1 matanlec2 alglec geomlec inflec difflec ) b07 b08 b09 b10 `(,a1,a1,a2,a3,a4,a4) schedclass '((matanlec1 matanlec2 alglec geomlec inflec difflec)))
;         
;           (sched '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan3 matan4 alg2 geom2 diff2 prog2 eng2)  (matan5 matan6 alg3 geom3 diff3 prog3 eng3) (matan7 matan8 alg4 geom4 diff4 prog4 eng4))
;                  `(,b07,b08,b09,b10) `((,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)(,a1,a1,a2,a3,a4,a5,a6)) `(,b1,b2,b3,b4)
;                  '((matan1 matan2 alg1 geom1 diff1 prog1 eng1 matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture)(matan3 matan4 alg2 geom2 diff2 prog2 eng2)(matan5 matan6 alg3 geom3 diff3 prog3 eng3)(matan7 matan8 alg4 geom4 diff4 prog4 eng4)))
;          (== q `(,b07,b08,b09,b10,teachersched,b5,schedclass,b6)))))
;это хорошо работающая функция для лекций и пар


(time (run 10 (q) (fresh (g1 g2 g3 g4 g5 g6 g7 a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b07new b08new b09new b10new c07new c08new c09new c10new b4 b5 b6 b7 b8 m1 m2 m3 m4 m5 m6 m7 m8 k1 k2 k3 k4 k5 k6 k7 k8 schedclass schedallgroup b07 b08 b09 b10 teachersched c1 c2 c3 c4 c07 c08 c09 c10) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched b07 b08 b09 b10 b1 b2 b3 b4) (init_manysched b5 b6 b7 b8 m1 m2 m3 m4) ;работает за 3,291 секунду, довольно ьыстро, но тут еще лекции можо ставитьь только в первый ри дня
                  (init_manysched c1 c2 c3 c4 c07 c08 c09 c10)
                  (== teachersched `(,a1,a2,a3,a4,a5,a6,a7,a8,c1)) (== schedclass `(,b1,b2,b3,b4,m1,m2,m3,m4))
                  (== b07new `(,b07 'b07)) (== b08new `(,b08 'b08)) (== b09new `(,b09 'b09)) (== b10new `(,b10 'b10)) (== c07new `(,c07 'c07)) (== c08new `(,c08 'c08)) (== c09new `(,c09 'c09)) (== c10new `(,c10 'c10))
                  (== c1 `(('window . ,k1) ('window . ,k2)  . ,k4)) (== c1 `((,g1 . ,g2) . ,g3)) (=/= g1 '()) ;тут я говорю, что лютому преподу во вторник нужно обязательно вставить пару.
                  (lec '(matanlec1 matanlec2 alglec1 geomlec1 inflec1 difflec1) b07 b08 b09 b10 `(,a1,a1,a2,a3,a4,a5) `(,b1,b2) '((matanlec1 matanlec2 alglec1 geomlec1 inflec1 difflec1) (matanlec1 matanlec2 alglec1 geomlec1 inflec1 difflec1)))
                  (lec '(matanlec3 matanlec4 alglec2 geomlec2 inflec2 difflec2) c07 c08 c09 c10 `(,a1,a1,a2,a3,a4,a5) `(,b1,b2) '((matanlec3 matanlec4 alglec2 geomlec2 inflec2 difflec2)(matanlec3 matanlec4 alglec2 geomlec2 inflec2 difflec2)))
                  (sched '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1)  (matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1)(matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5)   (matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5))
                 `(,b07new,b08new,b09new,b10new,c07new,c08new,c09new,c10new) `((,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)
                                                                                                      ) `(,b1,b2,b3,b4,m1,m2,m3,m4) '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1)  (matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan1 matan2 alg1 geom1 diff1 prog1 eng1)(matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5)))
                  
                 ; (sched '((matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5)   (matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5))
                  ;     `(,c07new,c08new,c09new,c10new) `((,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)(,c1,c1,a2,a3,a4,a5,a6)) `(,m1,m2,m3,m4) '((matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5) (matan9 matan10 alg5 geom5 diff5 prog5 eng5)))
                  
                  (== q `(,b07,b08,b09,b10,c07,c08,c09,c10,a1,a2,a3,a4,a5,a6,a7,a8,c1,b1,b2,b3,b4,m1,m2,m3,m4)))))
;почему-то считает целую минуту  2 находит за 50 секунд, 50 за 72 200 за 97; 10 cчитает за 52,  теперь 10 при объединении вовсе не работает, надо понять, почему
(run 5 (q) (=/= q 5))



(time (run 5 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8)
                        (sched '((matan1 matan2 matan3 matan4)(matan1 matan2 matan3 matan4)(matan1 matan2 matan3 matan4)) `((,a1 'a1)(,a2 'a2)(,a5 'a5)) `((,a3,a3,a3,a3)(,a3,a3,a3,a3)(,a3,a3,a3,a3)) `(,a4,a6) '((matan1 matan2 matan3 matan4)(matan1 matan2 matan3 matan4)))
                        (== q `(,a1,a2,a5,a3,a4,a6)))));почему то с тремя не работает


