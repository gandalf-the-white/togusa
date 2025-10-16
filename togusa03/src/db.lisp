(defpackage :db
  (:use :cl)
  (:export :init-db :*manifest-db*))

(in-package :db)


(defparameter *manifest-db* "wasmcloud_manifests.db")

(defun init-db (&optional (path *manifest-db*))
  (sqlite:with-open-database (db path)
    (sqlite:execute-non-query
     db
     "CREATE TABLE IF NOT EXISTS manifest (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         name TEXT,
         version TEXT,
         description TEXT
      );")
    (sqlite:execute-non-query
     db
     "CREATE TABLE IF NOT EXISTS cluster (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         name TEXT,
         nats_url TEXT,
         manifest_id INTEGER,
         FOREIGN KEY(manifest_id) REFERENCES manifest(id)
      );")
    (sqlite:execute-non-query
     db
     "CREATE TABLE IF NOT EXISTS component (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         name TEXT,
         image TEXT,
         replicas INTEGER,
         zone TEXT,
         manifest_id INTEGER,
         FOREIGN KEY(manifest_id) REFERENCES manifest(id)
      );")
    (sqlite:execute-non-query
     db
     "CREATE TABLE IF NOT EXISTS provider (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         name TEXT,
         capability TEXT,
         image TEXT,
         zone TEXT,
         manifest_id INTEGER,
         FOREIGN KEY(manifest_id) REFERENCES manifest(id)
      );")
    (sqlite:execute-non-query
     db
     "CREATE TABLE IF NOT EXISTS link (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         name TEXT,
         source TEXT,
         source_interface TEXT,
         target TEXT,
         target_interface TEXT,
         manifest_id INTEGER,
         FOREIGN KEY(manifest_id) REFERENCES manifest(id)
      );")
    (sqlite:execute-non-query
     db
     "CREATE TABLE IF NOT EXISTS spread (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         component_id INTEGER,
         name TEXT,
         weight INTEGER,
         zone TEXT,
         FOREIGN KEY(component_id) REFERENCES component(id)
      );")
    )
  (format t "Base SQLITE initialized!"))

