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

(define (insertmon1 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 cl1 cl2)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (caro h2 h3) (caro h3 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (caro schedonegroup a1) (caro a1 a2) (== a2 subj) ; поставил первую пару группе
            (caro classes cl1) (membero subj cl1) (caro schedclass k1) (caro k1 k2) (caro k2 k3) (== k3 subj) 
            )             
   ]
  
   ;бегаю по предметам учителей
  
  [(fresh (x1 x2 x3) (cdro schedclass x1) (cdro classes x2) (insertmon1 subjteacher teachersched subj schedonegroup x2 x1))]
    

     ;бегаю по доступным аудиториям
))

(define (inserttue1 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (caro h3 t1) (caro t1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (caro a1 t2) (caro t2 a2) (== a2 subj) ; поставил первую пару группе
              (caro classes cl1) (membero subj cl1)(caro schedclass k1) (cdro k1 k2) (caro k2 t3) (caro t3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (inserttue1 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))
(define (insertwed1 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (caro w1 t1) (caro t1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (caro w2 t2) (caro t2 a2) (== a2 subj) ; поставил первую пару группе
              (caro classes cl1) (membero subj cl1)(caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (caro w3 t3) (caro t3 k3) (== k3 subj) 
            )             
   ]
 ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertwed1 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertthu1 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (caro z1 t1) (caro t1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (caro z2 t2) (caro t2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (caro z3 t3) (caro t3 k3) (== k3 subj) 
            )             
   ]
   ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertthu1 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertfri1 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (cdro z1 x1) (caro x1 t1) (caro t1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (cdro z2 x2) (caro x2 t2) (caro t2 a2) (== a2 subj) ; поставил первую пару группе
              (caro classes cl1) (membero subj cl1)(caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (cdro z3 x3) (caro x3 t3) (caro t3 k3) (== k3 subj) 
            )             
   ]
   ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1) (cdro classes x2)(insertfri1 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertmon2 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 a4 a5 k1 k2 k3 k4 k5 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (caro h2 h3) (cdro h3 h4) (caro h4 h5) (== h5 subj) ;поставил соответствующему преподу первую пару
            (caro schedonegroup a1) (cdro a1 a2) (caro a2 a3) (== a3 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (caro k1 k2) (cdro k2 k3) (caro k3 k4) (== k4 subj) 
            )             
   ]
 ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1) (cdro classes x2)(insertmon2 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (inserttue2 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 b1 b2 b3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (caro h3 t1) (cdro t1 b1) (caro b1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (caro a1 t2) (cdro t2 b2) (caro b2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (caro k2 t3) (cdro t3 b3) (caro b3 k3) (== k3 subj) 
            )             
   ]
   ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (inserttue2 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertwed2 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (caro w1 t1) (cdro t1 z1) (caro z1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (caro w2 t2) (cdro t2 z2) (caro z2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (caro w3 t3) (cdro t3 z3) (caro z3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertwed2 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))


(define (insertthu2 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (caro z1 t1) (cdro t1 x1) (caro x1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (caro z2 t2) (cdro t2 x2) (caro x2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (caro z3 t3) (cdro t3 x3) (caro x3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertthu2 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertfri2 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 c1 c2 c3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (cdro z1 x1) (caro x1 t1) (cdro t1 c1) (caro c1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (cdro z2 x2) (caro x2 t2) (cdro t2 c2) (caro c2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (cdro z3 x3) (caro x3 t3) (cdro t3 c3) (caro c3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertfri2 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertmon3 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 h6 a1 a2 a3 a4 a5 a6 k1 k2 k3 k4 k5 k6 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (caro h2 h3) (cdro h3 h4) (cdro h4 h5) (caro h5 h6)  (== h6 subj) ;поставил соответствующему преподу первую пару
            (caro schedonegroup a1) (cdro a1 a2) (cdro a2 a3) (caro a3 a4) (== a4 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (caro k1 k2) (cdro k2 k3) (cdro k3 k4) (caro k4 k5) (== k5 subj) 
            )             
   ]
   ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertmon3 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (inserttue3 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 b1 b2 b3 n1 n2 n3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (caro h3 t1) (cdro t1 b1) (cdro b1 n1) (caro n1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (caro a1 t2) (cdro t2 b2) (cdro b2 n2) (caro n2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (caro k2 t3) (cdro t3 b3) (cdro b3 n3) (caro n3 k3) (== k3 subj) 
            )             
   ]
   ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (inserttue3 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertwed3 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (caro w1 t1) (cdro t1 z1) (cdro z1 x1) (caro x1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (caro w2 t2) (cdro t2 z2) (cdro z2 x2) (caro x2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (caro w3 t3) (cdro t3 z3) (cdro z3 x3) (caro x3 k3) (== k3 subj) 
            )             
   ]
 ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertwed3 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertthu3 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 c1 c2 c3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (caro z1 t1) (cdro t1 x1) (cdro x1 c1) (caro c1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (caro z2 t2) (cdro t2 x2) (cdro x2 c2) (caro c2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (caro z3 t3) (cdro t3 x3) (cdro x3 c3) (caro c3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertthu3 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertfri3 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 c1 c2 c3 v1 v2 v3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (cdro z1 x1) (caro x1 t1) (cdro t1 c1) (cdro c1 v1) (caro v1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (cdro z2 x2) (caro x2 t2) (cdro t2 c2) (cdro c2 v2) (caro v2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (cdro z3 x3) (caro x3 t3) (cdro t3 c3) (cdro c3 v3) (caro v3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertfri3 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))


(define (insertmon4 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 h6 h7 a1 a2 a3 a4 a5 a6 k1 k2 k3 k4 k5 k6 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (caro h2 h3) (cdro h3 h4) (cdro h4 h5) (cdro h5 h6) (caro h6 h7)  (== h7 subj) ;поставил соответствующему преподу первую пару
            (caro schedonegroup a1) (cdro a1 a2) (cdro a2 a3) (cdro a3 a4) (caro a4 a5) (== a5 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (caro k1 k2) (cdro k2 k3) (cdro k3 k4) (cdro k4 k5) (caro k5 k6) (== k6 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertmon4 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (inserttue4 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 b1 b2 b3 n1 n2 n3 m1 m2 m3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (caro h3 t1) (cdro t1 b1) (cdro b1 n1) (cdro n1 m1) (caro m1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (caro a1 t2) (cdro t2 b2) (cdro b2 n2) (cdro n2 m2) (caro m2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (caro k2 t3) (cdro t3 b3) (cdro b3 n3) (cdro n3 m3) (caro m3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (inserttue4 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))


(define (insertwed4 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 c1 c2 c3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (caro w1 t1) (cdro t1 z1) (cdro z1 x1) (cdro x1 c1) (caro c1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (caro w2 t2) (cdro t2 z2) (cdro z2 x2) (cdro x2 c2) (caro c2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (caro w3 t3) (cdro t3 z3) (cdro z3 x3) (cdro x3 c3) (caro c3 k3) (== k3 subj) 
            )             
   ]
 ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertwed4 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertthu4 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 c1 c2 c3 v1 v2 v3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (caro z1 t1) (cdro t1 x1) (cdro x1 c1) (cdro c1 v1) (caro v1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (caro z2 t2) (cdro t2 x2) (cdro x2 c2) (cdro c2 v2) (caro v2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (caro z3 t3) (cdro t3 x3) (cdro x3 c3) (cdro c3 v3) (caro v3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertthu4 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))

(define (insertfri4 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (h1 h2 h3 h4 h5 a1 a2 a3 k1 k2 k3 k4 t1 t2 t3 w1 w2 w3 z1 z2 z3 x1 x2 x3 c1 c2 c3 v1 v2 v3 b1 b2 b3 cl1)
            (caro subjteacher h1) (membero subj h1) (caro teachersched h2) (cdro h2 h3) (cdro h3 w1) (cdro w1 z1) (cdro z1 x1) (caro x1 t1) (cdro t1 c1) (cdro c1 v1) (cdro v1 b1) (caro b1 h4) (== h4 subj) ;поставил соответствующему преподу первую пару
            (cdro schedonegroup a1) (cdro a1 w2) (cdro w2 z2) (cdro z2 x2) (caro x2 t2) (cdro t2 c2) (cdro c2 v2) (cdro v2 b2) (caro b2 a2) (== a2 subj) ; поставил первую пару группе
             (caro classes cl1) (membero subj cl1) (caro schedclass k1) (cdro k1 k2) (cdro k2 w3) (cdro w3 z3) (cdro z3 x3) (caro x3 t3) (cdro t3 c3) (cdro c3 v3) (cdro v3 b3) (caro b3 k3) (== k3 subj) 
            )             
   ]
  ;бегаю по предметам учителей
  [(fresh (x1 x2 x3) (cdro schedclass x1)(cdro classes x2) (insertfri4 subjteacher teachersched subj schedonegroup x2 x1))] ;бегаю по доступным аудиториям
))





(define (insert2 subjteacher teachersched subj schedonegroup classes schedclass)

  (conde


   [(inserttue3 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(inserttue4 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertwed1 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertwed2 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertwed3 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertwed4 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertthu1 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertthu2 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertthu3 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertthu4 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertfri1 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertfri2 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertfri3 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertfri4 subjteacher teachersched subj schedonegroup classes schedclass)]
   
   [(insertmon1 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertmon2 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertmon3 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(insertmon4 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(inserttue1 subjteacher teachersched subj schedonegroup classes schedclass)]
   [(inserttue2 subjteacher teachersched subj schedonegroup classes schedclass)] 
   
   ))

(define (insert1 subjteacher teachersched subj schedonegroup classes schedclass)
  (conde
   [(fresh (b1 b2 b3) (caro subjteacher b1) (caro teachersched b2) (membero subj b1) (insert2 subjteacher teachersched subj schedonegroup classes schedclass))]
   [(fresh (a1 a2 a3) (cdro subjteacher a1) (cdro teachersched a2) (insert1 a1 a2 subj schedonegroup classes schedclass))]))



(define (timetableone subjteacher teachersched studyplanonegroup schedonegroup classes schedclass)
  (conde
   [(== studyplanonegroup '()) succeed]
   [(fresh (a1 a2 a3) (caro studyplanonegroup a1) (insert1 subjteacher teachersched a1 schedonegroup classes schedclass))
    (fresh (b1 b2) (cdro studyplanonegroup b1) (timetableone subjteacher teachersched b1 schedonegroup classes schedclass))]
   ))

(define (timetableall subjteacher teachersched studyplanallgroup schedallgroup classes schedclass)
  (conde
   [(== studyplanallgroup '()) succeed]
   [(fresh (a1 a2) (caro studyplanallgroup a1) (caro schedallgroup a2) (timetableone subjteacher teachersched a1 a2 classes schedclass))
    (fresh (b1 b2) (cdro studyplanallgroup b1) (cdro schedallgroup b2) (timetableall subjteacher teachersched b1 b2 classes schedclass))]))


(define (init_sched s) ; - расписание группы на неделю
  (fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9 b1 b2 b3 b4 b5 b6 b7 b8 b9 c1 c2 c3 c4 c5 c6) (== s `((,a1,a2,a3,a4)(,a5,a6,a7,a8)(,b1,b2,b3,b4)(,b5,b6,b7,b8)(,c1,c2,c3,c4)))))

(define (init_manysched a1 a2 a3 a4 a5 a6 a7 a8)
  (fresh (b1 b2 b3 b4 b5 b6 b7 b8) (init_sched b1) (init_sched b2) (init_sched b3) (init_sched b4) (init_sched b5) (init_sched b6) (init_sched b7) (init_sched b8) (== a1 b1) (== a2 b2) (== a3 b3) (== a4 b4) (== a5 b5) (== a6 b6) (== a7 b7) (== a8 b8)))


;(run 500 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 a9 k1 k2 k3 k4 k5 k6 k7 k8 k9 teachersched sched_allgroup schedclass m1 m2 n1 n2 n3 n4 n5 n6 l1 l2 m3 m4 l3 l4)
 ;                (== teachersched `(((,a1,a2)(,a3,a4)) ((,a5,a6)(,a7,a8)))) (== sched_allgroup `(((,k1,k2)(,k3,k4)) ((,k5,k6)(,k7,k8)))) (== schedclass `(((,m1,m2)(,m3,m4)) ((,l1,l2)(,l3,l4))))
  ;               (timetableall '((matan) (geom)) teachersched '((matan geom)) sched_allgroup '() schedclass) (== q `(,sched_allgroup,teachersched,schedclass))))
;(define (insertmon1 subjteacher teachersched subj schedonegroup classes schedclass)

;(run 5 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (== q `(,a1,a2))))



(define (inslecmon1 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 cl1 cl2)
       (caro first a1) (caro a1 a2) (== a2 lecture)
       (caro second b1) (caro b1 b2) (== b2 lecture)
       (caro third c1) (caro c1 c2) (== c2 lecture)
       (caro fourth v1) (caro v1 v2) (== v2 lecture)
       (caro teachersched x1) (caro x1 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (caro z2 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1) (cdro classes n2) (inslecmon1 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslecmon2 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 s1 s2 s3 s4 s5 s6 cl1 cl2)
       (caro first a1) (cdro a1 s1) (caro s1 a2) (== a2 lecture)
       (caro second b1) (cdro b1 s2) (caro s2 b2) (== b2 lecture)
       (caro third c1) (cdro c1 s3) (caro s3 c2) (== c2 lecture)
       (caro fourth v1) (cdro v1 s4) (caro s4 v2) (== v2 lecture)
       (caro teachersched x1) (cdro x1 s5) (caro s5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (cdro z2 s6) (caro s6 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslecmon2 lecture teachersched first second third fourth n2 n1))]
    ))
         
(define (inslecmon3 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 s1 s2 s3 s4 s5 s6 d1 d2 d3 d4 d5 d6 cl1 cl2)
       (caro first a1) (cdro a1 s1) (cdro s1 d1) (caro d1 a2) (== a2 lecture)
       (caro second b1) (cdro b1 s2) (cdro s2 d2) (caro d2 b2) (== b2 lecture)
       (caro third c1) (cdro c1 s3) (cdro s3 d3) (caro d3 c2) (== c2 lecture)
       (caro fourth v1) (cdro v1 s4) (cdro s4 d4) (caro d4 v2) (== v2 lecture)
       (caro teachersched x1) (cdro x1 s5) (cdro s5 d5) (caro d5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (cdro z2 s6) (cdro s6 d6) (caro d6 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslecmon3 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslecmon4 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 s1 s2 s3 s4 s5 s6 d1 d2 d3 d4 d5 d6 f1 f2 f3 f4 f5 f6 cl1 cl2)
       (caro first a1) (cdro a1 s1) (cdro s1 d1) (cdro d1 f1) (caro f1 a2) (== a2 lecture)
       (caro second b1) (cdro b1 s2) (cdro s2 d2) (cdro d2 f2) (caro f2 b2) (== b2 lecture)
       (caro third c1) (cdro c1 s3) (cdro s3 d3) (cdro d3 f3) (caro f3 c2) (== c2 lecture)
       (caro fourth v1) (cdro v1 s4) (cdro s4 d4) (cdro d4 f4) (caro f4 v2) (== v2 lecture)
       (caro teachersched x1) (cdro x1 s5) (cdro s5 d5) (cdro d5 f5) (caro f5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (caro z1 z2) (cdro z2 s6) (cdro s6 d6) (cdro d6 f6) (caro f6 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslecmon4 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslectue1 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 m1 m2 m3 m4 m5 m6 cl1 cl2)
       (cdro first m1) (caro m1 a1) (caro a1 a2) (== a2 lecture)
       (cdro second m2) (caro m2 b1) (caro b1 b2) (== b2 lecture)
       (cdro third m3) (caro m3 c1) (caro c1 c2) (== c2 lecture)
       (cdro fourth m4) (caro m4 v1)(caro v1 v2) (== v2 lecture)
       (cdro teachersched m5) (caro m5 x1) (caro x1 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
       (caro schedclass z1) (cdro z1 m6) (caro m6 z2) (caro z2 z3) (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslectue1 lecture teachersched first second third fourth n2 n1))]
    ))

(define (inslectue2 lecture teachersched first second third fourth classes schedclass)
   (conde
    [(fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 v1 v2 v3 v4 x1 x2 x3 x4 z1 z2 z3 z4 m1 m2 m3 m4 m5 m6 l1 l2 l3 l4 l5 l6 cl1 cl2)
       (cdro first m1) (caro m1 a1) (cdro a1 l1) (caro l1 a2) (== a2 lecture)
       (cdro second m2) (caro m2 b1) (cdro b1 l2) (caro l2 b2) (== b2 lecture)
       (cdro third m3) (caro m3 c1) (cdro c1 l3) (caro l3 c2) (== c2 lecture)
       (cdro fourth m4) (caro m4 v1)(cdro v1 l4) (caro l4 v2) (== v2 lecture)
       (cdro teachersched m5) (caro m5 x1) (cdro x1 l5) (caro l5 x2) (== x2 lecture)
       (caro classes cl1) (membero lecture cl1)
      (caro schedclass z1) (cdro z1 m6) (caro m6 z2) (cdro z2 l6) (caro l6 z3)  (== z3 lecture))
            ]
    [(fresh (n1 n2 n3) (cdro schedclass n1)(cdro classes n2) (inslectue2 lecture teachersched first second third fourth n2 n1))]
    ))


  
(define (inslecture2 lecture teachersched first second third fourth classes schedclass)
  (conde
   [(inslecmon1 lecture teachersched first second third fourth classes schedclass)]
   [(inslecmon2 lecture teachersched first second third fourth classes schedclass)]
   [(inslecmon3 lecture teachersched first second third fourth classes schedclass)]
   [(inslecmon4 lecture teachersched first second third fourth classes schedclass)]
   [(inslectue1 lecture teachersched first second third fourth classes schedclass)]
   [(inslectue2 lecture teachersched first second third fourth classes schedclass)]
   ))

(define (inslecture1 lecture subjteacher teachersched first second third fourth classes schedclass)
  (conde
   [(fresh (a1 a2 a3) (caro subjteacher a1) (membero lecture a1) (caro teachersched a2) (inslecture2 lecture a2 first second third fourth classes schedclass))]
   [(fresh (b1 b2 b3) (cdro subjteacher b1) (cdro teachersched b2) (inslecture1 lecture b1 b2 first second third fourth classes schedclass))]
   ))


(define (lecturefor4group lectureplan subjteacher teachersched first second third fourth classes schedclass)
  (conde
   [(== lectureplan '()) succeed]
   [(fresh (a1 a2 a3) (caro lectureplan a1) (inslecture1 a1 subjteacher teachersched first second third fourth classes schedclass))
    (fresh (b1 b2 b3) (cdro lectureplan b1) (lecturefor4group b1 subjteacher teachersched first second third fourth classes schedclass))]
   ))

;(run 5 (q) (fresh (b07 b08 b09 b10 a1 a2 a3 a4) (init_manysched b07 b08 b09 b10 a1 a2 a3 a4)
;     (lecturefor4group '(alg) '((alg)) `(,a3) b07 b08 b09 b10 '() `(,a2))
;     (== q `(,b07,b08,b09,b10,a3,a2))))
;(run 5 (q) (fresh (b07 b08 b09 b10 a1 a2 a3 a4 b1 b2) (init_manysched b07 b08 b09 b10 a1 a2 a3 a4)
;              (caro b07 b1) (caro b1 b2)     (== q b2)))

;(time (run 10 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 teachersched sched_allgroup schedclass b1 b2 b3 b4 b5 b6 b7 b8 subjteacher) (init_sched a1) (init_sched a2) (init_sched a3) (init_sched a4) (init_sched a5) (init_sched a6) (init_sched a7) (init_sched a8) (init_manysched b1 b2 b3 b4 b5 b6 b7 b8)
;     (== teachersched `(,a1,a2,a3,a4)) (== sched_allgroup `(,a5,a6,a7,a8)) (== schedclass `(,b1,b2,b3,b4))
;(lecturefor4group '(alglecture1 matanlecture1 matanlecture2 geomlecture1 englecture1)
;                  '((matan1 matan2 matan3 matan4 matan5 matan6 matan7 matan8 matan9 matan10 matan11 matan12 matan13 matan14 matan15 matan16 matanlecture1 matanlecture2) (alg1 alg2 alg3 alg4 alglecture1)(geom1 geom2 geom3 geom4 geomlecture1 ) (eng1 eng2 eng3 eng4 englecture1))
 ;                 teachersched a5 a6 a7 a8 '() schedclass)
 ;                 (timetableall '((matan1 matan2 matan3 matan4  matanlecture1 matanlecture2) (alg1 alg2 alg3 alg4 alglecture1)(geom1 geom2 geom3 geom4 geomlecture1 ) (eng1 eng2 eng3 eng4 englecture1))
 ;                teachersched '((matan1 matan2 alg1 geom1 eng1) (matan3 matan4 geom2 alg2 eng2) (matan5 matan6 geom3 alg3 eng3)(matan7 matan8 geom4 alg4 eng4)) sched_allgroup '() schedclass)
     
;    (== q `(,sched_allgroup,teachersched,schedclass)))))


;( time (run 2 (q) (fresh (b07 b08 b09 b10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 teachersched schedallgroup schedclass) (init_manysched b07 b08 b09 b10 a1 a2 a3 a4) (init_manysched a5 a6 a7 a8 a9 a10 a11 a12)
 ;                  (== teachersched `(,a1,a2,a3,a4,a5,a6)) (== schedallgroup `(,b07,b08,b09,b10)) (== schedclass `(,a7,a8,a9,a10))
  ;                 (timetableall '((proglecture prog1 prog2 prog3 prog4 prog5 prog6 prog7 prog8 prog9 prog10 prog11) (geomlecture geom1 geom2 geom3 geom4) ( matanlecture matan1 matan2 matan3 matan4) (alglecture alg1 alg2 alg3 alg4) () () ) teachersched '((matan1 alg1 geom1 prog1 prog5 )(matan2 alg2 geom2 prog2 prog6 )(matan3 alg3 geom3 prog3 prog7 )(matan4 alg4 geom4 prog4 prog8)) schedallgroup '() schedclass)
   ;                (lecturefor4group '(alglecture matanlecture geomlecture proglecture) '((proglecture) (geomlecture) ( matanlecture) (alglecture) () () ) teachersched b07 b08 b09 b10 '() schedclass)
                   
    ;               (== q `(,schedallgroup,teachersched,schedclass)))))

;работает, но за 14 секунд, но у каждой группы теперь уже по 10 предметов в неделю

;у тп-шек 6 лекций в неделю: 2 матана, геома, алг, диффуры, инфа, т е нужно создать минимум 6 функций для вставки лекций- сделано

;попробуем теперь составить само расписание

;матан практики у всех будет вести один и тот же человек, с практиками по другим предметам аналогично, только практик=лектор, т е у преподов будет по 5 пар в неделю
;английский у всех ведет тоже 1 человек, прогу тоже предметы матан, алг геом дифф англ прога
;lecture
      ;      (lecturefor4group '(matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture) '((matan1 matan2 matan3 matan4 matan5 matan6 matan7 matan8 matanlecture1 matanlecture2) (alg1 alg2 alg3 alg4 alglecture) (geom1 geom2 geom3 geom4 geomlecture)(eng1 eng2 eng3 eng4 eng5 eng6 eng7 eng8)(diff1 diff2 diff3 diff4 difflecture)(prog1 prog2 prog3 prog4) (inflecture))
      ;                        teachersched b07 b08 b09 b10 '() schedclass)
      ;      (== q `(,schedallgroup,teachersched,schedclass)))))

(+ 1 1)
;для групп находит довольно шустро (без англа за 4 сек), теперь добавил каждой группе по англу, выдало за 19 сек
;(lecturefor4group lectureplan subjteacher teachersched first second third fourth classes schedclass)
;добавил теперь 6 лекций, работает за 16 сек, но с учетом читерства, надо придумать способ ускорения для общего случая
;БЕЗ ЧИТЕРСТВА НЕ РАБОТАЕТ-не хватает памяти
; с ним, опять таки, за 16 - 22 сек для одного варианта, попробую 10. 10 нашел за 22 секунды;;для вывода можно использовать какуюнибудь html табличку

; сбор тредований
;  спрошу с помощью чего генерирует расписание Карташова,   до этого этим занимался Кольницкий (геометр)
;попробовать сходить на защиты кого-то    Ближайший план - написать по-человечески реляцию, которая вставляет пары (убрать мой копипаст)
;(timetableall subjteacher teachersched studyplanallgroup schedallgroup classes schedclass)
;(lecturefor4group lectureplan subjteacher teachersched first second third fourth classes schedclass)
(define (allrasp subjteacher teachersched studyplanallgroup schedallgroup classes schedclass lectureplan  first second third fourth)
(conde
 [(timetableall subjteacher teachersched studyplanallgroup schedallgroup classes schedclass) (lecturefor4group lectureplan subjteacher teachersched first second third fourth classes schedclass)]
))


(time (run 1 (q) (fresh (a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8 b07 b08 b09 b10 n1 n2 n3 n4 teachersched schedallgroup schedclass classes subjteacher studyplanallgroup lectureplan k1 k2 k3 k4 k5 k6 k7 k8) (init_manysched a1 a2 a3 a4 a5 a6 a7 a8) (init_manysched b1 b2 b3 b4 b5 b6 b7 b8) (init_manysched b07 b08 b09 b10 n1 n2 n3 n4)
            (== teachersched `(,a1,a2,a3,a4,a5,a6,a7)) (== schedallgroup `(,b07,b08,b09,b10)) (== schedclass `(,b1,b2,b3,b4,n1))
            (== classes '((matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture)(matan1 matan2 alg1 geom1 diff1 prog1 eng1 matanlecture1 matanlecture2 alglecture geomlecture difflecture inflecture)(matan3 matan4 alg2 geom2 diff2 prog2 eng2)(matan5 matan6 alg3 geom3 diff3 prog3 eng3)(matan7 matan8 alg4 geom4 diff4 prog4 eng4)))
            (== subjteacher '((matan1 matan2 matan3 matan4 matan5 matan6 matan7 matan8 matanlecture1 matanlecture2) (alg1 alg2 alg3 alg4 alglecture) (geom1 geom2 geom3 geom4 geomlecture)(eng1 eng2 eng3 eng4 eng5 eng6 eng7 eng8)(diff1 diff2 diff3 diff4 difflecture)(prog1 prog2 prog3 prog4) (inflecture)))
            (== studyplanallgroup '((matan1 matan2 alg1 geom1 diff1 prog1 eng1) (matan3 matan4 alg2 geom2 diff2 prog2 eng2) (matan5 matan6 alg3 geom3 diff3 prog3 eng3) (matan7 matan8 alg4 geom4 diff4 prog4 eng4)))
            (== lectureplan '(alglecture matanlecture1 matanlecture2 geomlecture difflecture inflecture))
            (allrasp subjteacher teachersched studyplanallgroup schedallgroup classes schedclass lectureplan b07 b08 b09 b10)
            (cdro a2 k1) (caro k1 k2) (cdro k2 k3) (cdro k3 k4) (caro k4 k5) (cdro k4 k6) (caro k6 k7) 
            (== q `(,schedallgroup,teachersched,schedclass)))))

; попробовал "заткнуть" некоторые пары у одного препода,  но программа работает слишком долго (не работает), а так работает приблизительно за 30 сек
