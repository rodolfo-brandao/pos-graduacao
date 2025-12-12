from sqlalchemy import create_engine, Column, Integer, String
from sqlalchemy.orm import declarative_base, sessionmaker
from sqlalchemy import text


Base = declarative_base()


class Aluno(Base):
    __tablename__ = 'aluno'
    id = Column(Integer, primary_key=True, autoincrement=True)
    nome = Column(String, nullable=False)
    idade = Column(Integer)


engine = create_engine('sqlite:///src///modulos///modulo-07///db///escola.db', echo=True)
Base.metadata.create_all(engine)
Session = sessionmaker(bind=engine)
session = Session()


if session.query(Aluno).count() == 0:
    session.add_all([
        Aluno(nome="Maria", idade=22),
        Aluno(nome="João", idade=25),
        Aluno(nome="Ana", idade=20)
    ])
    session.commit()


alunos = session.query(Aluno).all()
for aluno in alunos:
    print(aluno.id, aluno.nome, aluno.idade)


# Using ORM:
alunos = session.query(Aluno).filter(Aluno.idade >= 23).all()

for aluno in alunos:
    print(aluno.id, aluno.nome, aluno.idade)


# Using raw SQL:
result = session.execute(text("SELECT * FROM aluno WHERE idade >= 23;"))

for row in result:
    print(row.id, row.nome, row.idade)
