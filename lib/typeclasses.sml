signature SHOW =
   sig
      type t
      val show : t -> string
   end

signature EQ =
   sig
      type t
      val eq : t * t -> bool
   end
