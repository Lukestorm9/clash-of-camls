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
  highest_uuid : int ref;
  mutex : Mutex.t;
}
