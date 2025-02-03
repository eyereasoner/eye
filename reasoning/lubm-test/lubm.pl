% lubm rules

:- op(1200, xfx, :+).

'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Employee>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/AdministrativeStaff>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/advisor>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.example.org/advisor>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.example.org/affiliatedOrganizationOf>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.example.org/affiliatedOrganizationOf>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.example.org/affiliateOf>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/affiliateOf>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/age>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Article>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/AssistantProfessor>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/AssociateProfessor>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Book>').
'<http://www.example.org/headOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Department>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Chair>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Chair>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Chair>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/AdministrativeStaff>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/ClericalStaff>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/College>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Article>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/ConferencePaper>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Work>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Course>').
'<http://www.example.org/headOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/College>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Dean>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Dean>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/degreeFrom>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/University>') :+ '<http://www.example.org/degreeFrom>'(_, A).
'<http://www.example.org/hasAlumnus>'(A, B) :+ '<http://www.example.org/degreeFrom>'(B, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Department>').
'<http://www.example.org/headOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Program>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Director>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Director>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/doctoralDegreeFrom>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/University>') :+ '<http://www.example.org/doctoralDegreeFrom>'(_, A).
'<http://www.example.org/degreeFrom>'(A, B) :+ '<http://www.example.org/doctoralDegreeFrom>'(A, B).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/emailAddress>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Employee>').
'<http://www.example.org/worksFor>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Organization>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Employee>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Employee>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Faculty>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/FullProfessor>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Course>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/GraduateCourse>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/GraduateStudent>').
'<http://www.example.org/takesCourse>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/GraduateCourse>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/GraduateStudent>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/hasAlumnus>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/University>') :+ '<http://www.example.org/hasAlumnus>'(A, _).
'<http://www.example.org/degreeFrom>'(A, B) :+ '<http://www.example.org/hasAlumnus>'(B, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Dean>') :+ '<http://www.example.org/headOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/College>').
'<http://www.example.org/worksFor>'(A, B) :+ '<http://www.example.org/headOf>'(A, B).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Institute>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Article>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/JournalArticle>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Faculty>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Lecturer>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Course>') :+ '<http://www.example.org/listedCourse>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Schedule>') :+ '<http://www.example.org/listedCourse>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Manual>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/mastersDegreeFrom>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/University>') :+ '<http://www.example.org/mastersDegreeFrom>'(_, A).
'<http://www.example.org/degreeFrom>'(A, B) :+ '<http://www.example.org/mastersDegreeFrom>'(A, B).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.example.org/member>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/member>'(_, A).
'<http://www.example.org/memberOf>'(A, B) :+ '<http://www.example.org/member>'(B, A).
'<http://www.example.org/member>'(A, B) :+ '<http://www.example.org/memberOf>'(B, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.example.org/orgPublication>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.example.org/orgPublication>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Chair>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>'), '<http://www.example.org/headOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Department>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Director>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>'), '<http://www.example.org/headOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Program>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Student>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>'), '<http://www.example.org/takesCourse>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Course>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/TeachingAssistant>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>'), '<http://www.example.org/teachingAssistantOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Course>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Employee>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>'), '<http://www.example.org/worksFor>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Organization>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Faculty>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/PostDoc>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Faculty>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Program>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/publicationAuthor>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.example.org/publicationAuthor>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.example.org/publicationDate>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.example.org/publicationResearch>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Research>') :+ '<http://www.example.org/publicationResearch>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Work>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Research>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/ResearchAssistant>').
'<http://www.example.org/worksFor>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/ResearchGroup>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/ResearchAssistant>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/ResearchGroup>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Research>') :+ '<http://www.example.org/researchProject>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/ResearchGroup>') :+ '<http://www.example.org/researchProject>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Software>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.example.org/softwareDocumentation>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Software>') :+ '<http://www.example.org/softwareDocumentation>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Software>') :+ '<http://www.example.org/softwareVersion>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Specification>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Student>').
'<http://www.example.org/takesCourse>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Course>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Student>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.example.org/subOrganizationOf>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.example.org/subOrganizationOf>'(_, A).
'<http://www.example.org/subOrganizationOf>'(A, B) :+ '<http://www.example.org/subOrganizationOf>'(A, C), '<http://www.example.org/subOrganizationOf>'(C, B).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/AdministrativeStaff>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/SystemsStaff>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Course>') :+ '<http://www.example.org/teacherOf>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Faculty>') :+ '<http://www.example.org/teacherOf>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/TeachingAssistant>').
'<http://www.example.org/teachingAssistantOf>'(A, B), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(B, '<http://www.example.org/Course>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/TeachingAssistant>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Course>') :+ '<http://www.example.org/teachingAssistantOf>'(_, A).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/TeachingAssistant>') :+ '<http://www.example.org/teachingAssistantOf>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Article>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/TechnicalReport>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/telephone>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.example.org/tenured>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/title>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Person>') :+ '<http://www.example.org/undergraduateDegreeFrom>'(A, _).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/University>') :+ '<http://www.example.org/undergraduateDegreeFrom>'(_, A).
'<http://www.example.org/degreeFrom>'(A, B) :+ '<http://www.example.org/undergraduateDegreeFrom>'(A, B).
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Student>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/UndergraduateStudent>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Organization>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/University>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Publication>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/UnofficialPublication>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/Professor>') :+ '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(A, '<http://www.example.org/VisitingProfessor>').
'<http://www.example.org/memberOf>'(A, B) :+ '<http://www.example.org/worksFor>'(A, B).

% query
true :+ '<http://www.example.org/degreeFrom>'(_, _).
