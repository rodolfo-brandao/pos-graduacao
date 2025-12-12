import sqlite3
from sqlalchemy import create_engine, Column, Integer, String
from sqlalchemy.orm import declarative_base, sessionmaker


Base = declarative_base()


class Aluno(Base):
    __tablename__ = 'aluno'
    id = Column(Integer, primary_key=True, autoincrement=True)
    nome = Column(String, nullable=False)
    idade = Column(Integer)


conn = sqlite3.connect('src///modulos///modulo-07///db///escola.db')
cursor = conn.cursor()


cursor.execute("INSERT INTO Aluno (nome, idade) VALUES (?, ?)", ("Roberta", 30))
conn.commit()
conn.close()


engine = create_engine('sqlite:///src///modulos///modulo-07///db///escola.db', echo=True)


Session = sessionmaker(bind=engine)
session = Session()


alunos = session.query(Aluno).all()
for aluno in alunos:
    print(aluno.id, aluno.nome, aluno.idade)
