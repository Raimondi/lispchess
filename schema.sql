CREATE TABLE library( row integer primary key , depth integer , halfmoves integer , position string , move string);
CREATE INDEX ld on library(depth);
CREATE INDEX lh on library(halfmoves);
CREATE INDEX lp on library(position);

