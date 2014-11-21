(deftable ship ()
  (name :type text :indexp t)
  (flag :type text :nullp nil)
  (tonnage :type double :nullp nil)
  (length :type integer :nullp nil))

(create 'ship :name "Mærsk Mc-Kinney Møller")
