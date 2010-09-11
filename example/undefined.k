(if (undefined? trace)
    (trace "** ERROR: defined? doesn't work **")
    (trace "Hello, World!"))

(if (undefined? no-one-knows)
    (trace "Hello, World!")
    (trace "** ERROR: defined? doesn't work **"))


