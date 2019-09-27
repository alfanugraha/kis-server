CREATE TABLE public.approval
(
  id integer NOT NULL,
  unique_id character varying(256),
  file_name character varying(256),
  kugi_name character varying(256),
  individual_name character varying(50),
  organisation_name character varying(50),
  position_name character varying(50),
  status character varying(50),
  approval character varying(256),
  rejection character varying(256),
  CONSTRAINT id_pkey PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE public.approval
  OWNER TO postgres;