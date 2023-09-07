import hashlib


# The parameters are chosen so that this is a "proper" (not necessarily safe) ECDSA parameter.

## a=3, b=174  -> group is of prime order.
gf181 = 1552511030102430251236801561344621993261920897571225601

F = FiniteField(gf181)
a  = 3
b  = 174
E  = EllipticCurve(F, [a, b])
G  = E.gens()[0]
n  = G.order()
h  = 1
Fn = FiniteField(n)

print(n)


def hashit(msg):
  return Integer('0x' + hashlib.sha256(msg.encode()).hexdigest())

def keygen():
  d = randint(1, n - 1)
  Q = d * G
  return (Q, d)

def ecdsa_sign(d, m):
  r = 0
  s = 0
  while s == 0:
    k = 1
    while r == 0:
      k = randint(1, n - 1)
      Q = k * G
      (x1, y1) = Q.xy()
      r = Fn(x1)
    e = hashit(m)
    s = Fn(k) ^ (-1) * (e + d * r)
  return [r, s]

def ecdsa_verify(Q, m, r, s):
  e = hashit(m)
  w = s ^ (-1)
  u1 = (e * w)
  u2 = (r * w)
  P1 = Integer(u1) * G
  P2 = Integer(u2) * Q
  X = P1 + P2
  (x, y) = X.xy()
  v = Fn(x)
  return v == r


(Q, d) = keygen()
m = 'My Message'

[r, s] = ecdsa_sign(d, m)
result = ecdsa_verify(Q, m, r, s)

print (f"Message: {m}")
print (f"Public Key: {Q.xy()}")
print (f"Private Key: {d}")

print ("=== Signature ===")
print (f" r = {r}")
print (f" s = {s}")
print (f"Verification: {result}")