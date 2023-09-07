import hashlib


# The parameters are chosen so that this is a "proper" (not necessarily safe) ECDSA parameter.
p = 2833

F = FiniteField(p)
a  = 1
b  = 1
E  = EllipticCurve(F, [a, b])
# G  = E.gens()[0]
G = E(1341,854)
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

msg_hash = Fn(hashit(m))

print (f"Message: {m}")
print (f"Message hash : {msg_hash}")
print (f"Public Key: {Q.xy()}")
print (f"Private Key: {d}")

print ("=== Signature ===")
print (f" r = {r}")
print (f" s = {s}")
print (f"Verification: {result}")

from random import randbytes

v = []

for i in range(30):
  m = str(randbytes(4))
  [r, s] = ecdsa_sign(d, m)
  result = ecdsa_verify(Q, m, r, s)

  msg_hash = Fn(hashit(m))
  v.append((r,s,msg_hash))
print(v)