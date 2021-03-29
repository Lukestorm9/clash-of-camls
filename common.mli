type entity = {
  uuid : int;
  x : float;
  y : float;
  vx : float;
  vy : float;
  time_sent_over : float;
  graphic : string;
  health : float;
}

type world_state = {
  data : entity option array;
  mutex : Mutex.t;
  uuid : int option ref;
}
