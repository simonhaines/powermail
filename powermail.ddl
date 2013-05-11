CREATE TABLE Users
  (Id CHAR(32) NOT NULL,
   DateFormat CHAR(3) NOT NULL,
   Timezone VARCHAR(32) NOT NULL,
   TimeRef TIME NOT NULL,
   PRIMARY KEY (Id));

CREATE TABLE Contacts
  (Id CHAR(32) NOT NULL,
   Owner CHAR(32) NULL,
   Name NVARCHAR(128) NULL,
   Email VARCHAR(128) NULL,
   Phone VARCHAR(32) NULL,
   PRIMARY KEY (Id)
   FOREIGN KEY (Owner) REFERENCES Users (Id));

CREATE INDEX ContactsEmail ON Contacts (Email);

CREATE TABLE Associates
  (Owner CHAR(32) NOT NULL,
   Contact CHAR(32) NOT NULL,
   PRIMARY KEY (Owner, Contact),
   FOREIGN KEY (Owner) REFERENCES Users (Id),
   FOREIGN KEY (Contact) REFERENCES Contacts (Id));

CREATE TABLE Events
  (Id CHAR(32) NOT NULL,
   Owner CHAR(32) NOT NULL,
   TimeUTC DATETIME NOT NULL,
   Content TEXT NOT NULL,
   PRIMARY KEY (Id),
   FOREIGN KEY (Owner) REFERENCES Users (Id));

CREATE INDEX EventTime ON Events (TimeUTC);

CREATE TABLE Recipients
  (Event CHAR(32) NOT NULL,
   Contact CHAR(32) NOT NULL,
   PRIMARY KEY (Event, Contact),
   FOREIGN KEY (Event) REFERENCES Events (Id),
   FOREIGN KEY (Contact) REFERENCES Contacts (Id));

CREATE TABLE Tags
  (Id CHAR(32) NOT NULL,
   Name NVARCHAR(64) NOT NULL,
   PRIMARY KEY (Id));

CREATE TABLE EventTags
  (Event CHAR(32) NOT NULL,
   Tag CHAR(32) NOT NULL,
   PRIMARY KEY (Event, Tag),
   FOREIGN KEY (Event) REFERENCES Events (Id),
   FOREIGN KEY (Tag) REFERENCES Tags (Id));
