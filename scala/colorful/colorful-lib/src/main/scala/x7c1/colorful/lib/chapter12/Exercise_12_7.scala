package x7c1.colorful.lib.chapter12

object Exercise_12_7 {
  /*
    Left identity
      map2(unit(()), fa)((_,a) => a) == fa
      --> flatMap(unit(())){x => map(fa){y => y}} == fa
      --> flatMap(unit(())){x => fa} == fa
      --> compose(_ => unit(), _ => fa)(()) == fa
      --> compose(unit, _ => fa)(()) == fa
      --> {_ => fa}(()) == fa
      --> fa == fa

    Right identity
      map2(fa, unit(()))((a,_) => a) == fa
      --> flatMap(fa){x => map(unit(())){y => x}} == fa
      --> flatMap(fa){x => map(unit(())){y => x}} == fa
      --> flatMap(fa){x => unit(x)} == fa
      --> compose(_ => fa, {x => unit(x)})(()) == fa
      --> compose(_ => fa, unit)(()) == fa
      --> {_ => fa}(()) == fa
      --> fa == fa

    Associativity
      product(product(fa,fb),fc)
      --> product(map2(fa, fb){(_, _)},fc)
      --> map2(map2(fa, fb){(_, _)}, fc){(_, _)}
      --> flatMap(map2(fa, fb){(_, _)}){ x => map(fc){ y => (x, y) } }
      --> flatMap(flatMap(fa){x => map(fb)[y => (x, y)] }){
            x => map(fc){ y => (x, y) }
          }
      --> flatMap(fa){a =>
            flatMap(map(fb){y => (a, y)}){
              x => map(fc){ y => (x, y) }
            }
          }
      --> flatMap(fa){a =>
            flatMap(flatMap(fb){b => unit((a, b))}){
              x => map(fc){ y => (x, y) }
            }
          }
      --> flatMap(fa){a =>
            flatMap(flatMap(fb){b => unit((a, b))}){
              x => flatMap(fc){c => unit((x, c))}
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(unit((a, b))){
                x => flatMap(fc){c => unit((x, c))}
              }
            }
          }
   [1]--> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(fc){c =>
                unit(((a, b), c))
              }
            }
          }

      product(fa, product(fb,fc))
      --> product(fa, map2(fb, fc){(_, _)})
      --> map2(fa, map2(fb, fc){(_, _)}){(_, _)}
      --> map2(fa, flatMap(fb){b => map(fc){c => (b, c)}}){(_, _)}
      --> flatMap(fa){a =>
            map(flatMap(fb){b => map(fc){c => (b, c)}})){x => (a, x)}
          }
      --> flatMap(fa){a =>
            flatMap(flatMap(fb){b => map(fc){c => (b, c)}})){x => unit((a, x))}
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(map(fc){c => (b, c)}){
                x => unit((a, x))
              }
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(flatMap(fc){c => unit((b, c))}){
                x => unit((a, x))
              }
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(fc){c =>
                flatMap(unit((b, c))){
                  x => unit((a, x))
                }
              }
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(fc){c =>
                unit((a, (b, c)))
              }
            }
          }

      map(product(fa, product(fb,fc)))(assoc)
      --> flatMap(flatMap(fa)(a =>..)){x => unit(assoc(x))}
      --> flatMap(fa){a =>
            flatMap(flatMap(fb){b =>..}){x => unit..}
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(flatMap(fc){c =>..}){x => unit..}
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(fc){c =>
                flatMap(unit((a, (b, c)))){x => unit(assoc(x))}
              }
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(fc){c =>
                unit(assoc((a, (b, c))))
              }
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(fc){c =>
                unit(((a, b), c))
              }
            }
          }
      --> [1]

    Naturality
      map2(fa,fb)(productF(f,g))
      --> flatMap(fa){x => map(fb){y => productF(f,g)(x,y)}}
      --> flatMap(fa){x => map(fb){y => (f(x), g(y)) }}
      --> flatMap(fa){x => flatMap(fb){y => unit( (f(x), g(y)) )}}
   [2]--> flatMap(fa){a =>
            flatMap(fb){b =>
              unit((f(a), f(b)))
            }
          }


      product(map(fa)(f), map(fb)(g))
      --> map2(map(a)(f), map(b)(g)){(_, _)}
      --> flatMap(map(a)(f)){x => map(map(b)(g)){y => (x, y)}}

      --> product(
            flatMap(fa){a => unit(f(a))},
            flatMap(fb){b => unit(f(b))}
          )
      --> map2(flatMap(fa){a => unit(f(a))}, flatMap(fb){b => unit(f(b))}){(_, _)}
      --> flatMap(flatMap(fa){a => unit(f(a))}){x =>
            map(flatMap(fb){b => unit(f(b))}){y =>
              (x, y)
            }
          }
      --> flatMap(flatMap(fa){a => unit(f(a))}){x =>
            flatMap(flatMap(fb){b => unit(f(b))}){y =>
              unit((x, y))
            }
          }
      --> flatMap(fa){a =>
            flatMap(unit(f(a))){x =>
              flatMap(flatMap(fb){b => unit(f(b))}){y =>
                unit((x, y))
              }
            }
          }
      --> flatMap(fa){a =>
            flatMap(flatMap(fb){b => unit(f(b))}){y =>
              unit((f(a), y))
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              flatMap(unit(f(b))){y =>
                unit((f(a), y))
              }
            }
          }
      --> flatMap(fa){a =>
            flatMap(fb){b =>
              unit((f(a), f(b)))
            }
          }
      --> [2]

  [2]'--> for{
            a <- fa
            b <- fb
          } yield (f(a), f(b))

   */
}
