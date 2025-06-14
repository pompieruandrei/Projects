#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <string.h>
#include<vector>
#include<algorithm>
#include<string>

using namespace std;

class Persoana {
private:
	char* nume = nullptr;
	int varsta = 0;
public:
	Persoana() {

	}
	Persoana(const char* nume, int varsta) {
		this->varsta = varsta;
		this->nume = new char[strlen(nume) + 1];
		strcpy(this->nume, nume);
	}
	Persoana(const Persoana& copie) {
		this->varsta = copie.varsta;
		this->nume = new char[strlen(copie.nume) + 1];
		strcpy(this->nume, copie.nume);
	}
	friend ostream& operator <<(ostream& out, const Persoana& k) {
		if (k.nume != nullptr) {
			out <<endl << "Numele persoanei este  " << k.nume;
		}
		out << " si are varsta  " << k.varsta << " ani";
		return out;
	}
	friend istream& operator >>(istream& in, Persoana& k) {
		char aux[100];
		cout << endl << "Nume Persoana : ";
		in.ignore();
		in.getline(aux, 100);
		if (k.nume) {
			delete[]k.nume;
		}
		k.nume = new char[strlen(aux)+1];
		strcpy(k.nume, aux);
		cout << endl << "Are varsta ";
		in >> k.varsta;
		return in;
	}
	int getvarsta() {
		return this->varsta;
	}
	void setvarsta(int varsta) {
		this->varsta = varsta;
	}
	const char* getNume()const {
		if (nume == nullptr) {
			throw  exception("Numele nu exista ");
		}
		if (nume != nullptr) {
			return this->nume;
		}
	}
	void setNume(const char* nume) {
		if (this->nume) {
			delete[]this->nume;
		}
		if (nume != nullptr) {
			this->nume = new char[strlen(nume) + 1];
			strcpy(this->nume, nume);
		}
	}
		Persoana& operator=(const Persoana& copie) {
				delete[]nume;
				this->varsta = copie.varsta;
				this->nume = new char[strlen(copie.nume) + 1];
				strcpy(this->nume, copie.nume);
			
			return *this;
	}
	
	~Persoana() {
		if (nume) {
			delete[] nume;
		}
		cout << endl<< "S-a apelat destrutorul ";
	}
};
class Angajat:public Persoana {
private:
	string nume_manager = "Mihai";
	double salariu = 100;
	int id_angajat = 0;
	static const double salariu_minim;
public:
	Angajat() :Persoana() {

	}
	Angajat(const char* nume,int varsta,string nume_manager, double salariu, int id_angajat) :Persoana(nume,varsta) {
		this->nume_manager = nume_manager;
		this->salariu = salariu;
		this->id_angajat = id_angajat;
		setSalariu(salariu);
	}
	Angajat(const Angajat& copie) :Persoana( copie) {
		this->nume_manager = copie.nume_manager;
		this->salariu = copie.salariu;
		this->id_angajat = copie.id_angajat;
	}
	string getnume_manager() {
		return this->nume_manager;
	}
	void setnume_manager(string nume_manager) {
		this->nume_manager = nume_manager;
	}
	double getsalariu()const {
		return this->salariu;
	}
	void setsalariu(double salariu) {
		if (salariu == -1) {
			throw exception("Nu are cum sa aiba salariu negativ");
		}
		this->salariu = salariu;
	}
	int getId_angajat() {
		if (id_angajat == 0) {
			throw exception("!!!!!!!!!!!! Acest angajat este facut cu construtorul implicit nu este valabil");
		}
		return this->id_angajat;
	}
	void setId_angajat(int id_angajat) {
		this->id_angajat = id_angajat;
	}
	void setSalariu(double salariu) {
		if (salariu < salariu_minim) {
			throw exception("Salariu este sub salariul minim !");
		}
		this->salariu = salariu;
	}
	friend ostream& operator<<(ostream& out,const  Angajat& k) {
		out << "ID_angajat :  " << k.id_angajat;
		out  << static_cast<const Persoana&>(k);
		out << " numele Manegerelui este " << k.nume_manager;
		out << " si are salariul de  " << k.salariu;
		return out;
	}
	friend istream& operator>>(istream& in, Angajat& k) {
		cout << endl << "ID-ul anagajatului este ";
		in >> k.id_angajat;
		in.ignore();
		in >> static_cast<Persoana&>(k);
		cout << endl << "Numele Managerului este  ";
		getline(in, k.nume_manager);
		in >> k.nume_manager;
		cout << endl << "Salariul este  ";
		in >> k.salariu;
		return in;
	}
	bool comparareSalarii(const Angajat& p, const Angajat& k) {
		return p.getsalariu() > k.getsalariu();
	}
};
const double Angajat::salariu_minim = 1500.0;

enum POST {
	fullstack,
	backend, 
	frontend
};
class Programator : public Angajat {
private:
	char* limbajProgramareFolosit = nullptr;
	POST post=fullstack;
public:
	Programator() :Angajat(){

	}
	Programator(const char* nume, int varsta, string nume_manager, double salariu, int id_angajare, const char* limbajProgramareFolosit, POST post) :Angajat(nume, varsta, nume_manager, salariu, id_angajare) {
		this->post = post;
		this->limbajProgramareFolosit = new char[strlen(limbajProgramareFolosit) + 1];
		strcpy(this->limbajProgramareFolosit, limbajProgramareFolosit);
	}
	Programator(const Programator& copie) :Angajat(copie) {
		this->post = copie.post;
		this->limbajProgramareFolosit = new char[strlen(copie.limbajProgramareFolosit) + 1];
		strcpy(this->limbajProgramareFolosit, copie.limbajProgramareFolosit);
	}
	Programator& operator=(const Programator& copie) {
		if (this != &copie) {
			Angajat::operator=(copie);
			if (limbajProgramareFolosit) {
				delete[]limbajProgramareFolosit;
			}
			if (copie.limbajProgramareFolosit) {
				limbajProgramareFolosit = new char[strlen(copie.limbajProgramareFolosit) + 1];
				strcpy(limbajProgramareFolosit, copie.limbajProgramareFolosit);
			}
			post = copie.post;
		}
		return *this;
	}
	friend ostream& operator<<(ostream& out,  const Programator& p) {
		out << "Programator " << endl;
		out << static_cast< const Angajat&>(p);
		if (p.limbajProgramareFolosit != nullptr) {
			out << " limbajul folosit este " << p.limbajProgramareFolosit;
		}
		out << " si are postul de " << p.post;
		return out;
	}
	friend istream& operator>>(istream& in,Programator& k){
		in >> static_cast<Programator&>(k);
		char buffer[100];
		cout << "Limbajul folosit ";
		in.getline(buffer, 100);
		if (k.limbajProgramareFolosit) {
			delete[]k.limbajProgramareFolosit;
		}
		k.limbajProgramareFolosit = new char[strlen(buffer) + 1];
		strcpy(k.limbajProgramareFolosit, buffer);
		int post_value;
		cout << endl << "Si are postul (0 , 1 , 2 ) ";
		in >> post_value;
		if (post_value < 0 || post_value>2) {
			throw invalid_argument("Valoare invalida ");
		}
		k.post = static_cast<POST>(post_value);
		return in;

	}
	const char* getlimbajProgramareFolosit() {
		if (limbajProgramareFolosit != nullptr) {
			return this->limbajProgramareFolosit;
			
		}
	}
	POST getpost() {
		return this->post;
	}
	void setPost(POST post) {
		if (post == backend) {
			throw exception("!!!!!!!!!!!! Esti sigur ca acesta este postul ?");
		}
		this->post = post;
	}

	void setlimbajProgramareFolosit(const char* limbajProgramareFolosit) {
		if (this->limbajProgramareFolosit ) {
			delete[]this->limbajProgramareFolosit;
		}
		if (limbajProgramareFolosit != nullptr) {
			this->limbajProgramareFolosit = new char[strlen(limbajProgramareFolosit) + 1];
			strcpy(this->limbajProgramareFolosit, limbajProgramareFolosit);
		}
		}
	~Programator(){
		if (limbajProgramareFolosit) {
			delete[] limbajProgramareFolosit;
		}
		cout << endl << "S-a apelat destructorul 2";
	}
};
class Departament {
private: 
	char* denumire = nullptr;
	Angajat** angajati=nullptr;
	int nr_angajati = 0;
	static int nr_total_angajati;
public:
	Departament() {

	}
	Departament(const char* denumire) {
		if (denumire != nullptr) {
			this->denumire = new char[strlen(denumire) + 1];
			strcpy(this->denumire, denumire);
		}
		angajati = nullptr;
		nr_angajati = 0;
	}
	Departament(const Departament& copie) {
		if (copie.denumire != nullptr) {
			this->denumire = new char[strlen(copie.denumire) + 1];
			strcpy(this->denumire, copie.denumire);
		}
		this->nr_angajati = copie.nr_angajati;
		if (nr_angajati > 0) {
			angajati = new Angajat * [nr_angajati];
			for (int i = 0; i < nr_angajati; i++) {
				angajati[i] = new Angajat(*copie.angajati[i]);
			}
		}
		else {
			angajati = nullptr;
		}
	}
	Departament& operator=(const Departament& copie) {
		if (this!=&copie) {
			if (this->denumire) {
				delete[]denumire;
			}
			if (copie.denumire != nullptr) {
				this->denumire = new char[strlen(copie.denumire) + 1];
				strcpy(this->denumire, copie.denumire);
			}
			for (int i = 0; i < nr_angajati; i++) {
				delete angajati[i];
			}
			delete[]angajati;

			this->nr_angajati = copie.nr_angajati;
			if (nr_angajati > 0) {
				this->angajati = new Angajat * [nr_angajati];
				for (int i = 0; i < nr_angajati; i++) {
					angajati[i] = new Angajat(*copie.angajati[i]);
				}
			}
			else {
				angajati = nullptr;
			}
		}
		return *this;
	}
	void adaugaAngajat(Angajat* angajat) {
		Angajat** temp = new Angajat * [nr_angajati + 1];
		for (int i = 0; i < nr_angajati; i++) {
			temp[i] = angajati[i];
		}
		temp[nr_angajati] = angajat;
		delete[]angajati;
		angajati = temp;
		nr_angajati++;
		nr_total_angajati++;
	}
	static int getNrTotalAngajati() {
		return nr_total_angajati;
	}
	~Departament() {
		if (denumire) {
			delete[]denumire;
		}
		for (int i = 0; i < nr_angajati; i++) {
			if (angajati[i]) {
				delete angajati[i];
			}
		}
		if (angajati) {
			delete[]angajati;
		}
		cout << endl << "S-a apelat destrutorul 3" ;
	}
	friend ostream& operator<<(ostream& out,const Departament k) {
		if (k.denumire != nullptr) {
			out <<endl<< "Denumirea departamentului este " << k.denumire;
		}
		out <<endl<< " Numarul Angajatilor este " << k.nr_angajati;
		for (int i = 0; i < k.nr_angajati; i++) {
			out <<endl<< "~~~~~~~~~~~~~~~~~~~~~"<<endl;
			out << "Angajat " << i + 1 << endl << *k.angajati[i];
		}
		return out;
	}
	friend istream& operator>>(istream& in, Departament k) {
		char buffer2[100];
		cout << "Denumirea departamentului este  ";
		in.ignore();
		 in.getline(buffer2, 100);
		 if (k.denumire) {
			 delete[]k.denumire;
		 }
		 k.denumire = new char[strlen(buffer2) + 1];
		 strcpy(k.denumire, buffer2);
		 cout << "Numarul angajatilor ";
		 in >> k.nr_angajati;
		 k.angajati = new Angajat * [k.nr_angajati];
		 for (int i = 0; i < k.nr_angajati; i++) {
			 cout << endl << "Detalii angajati  " << i + 1 << ":" << endl;
			 k.angajati[i] = new Angajat();
			 in >> *k.angajati[i];
		 }
		 return in;
	}
	double salariuTotal() {
		double total = 0;
		for (int i = 0; i < nr_angajati; i++) {
			total += angajati[i]->getsalariu();
		}
		return total;
	}
	void cautaAngajatDupaNume(const char* nume) {
		bool gasit = false;
		for (int i = 0; i < nr_angajati; i++) {
			if (strcmp(angajati[i]->getNume(), nume) == 0) {
				cout << "Angajat gasit: " << *angajati[i] << endl;
				gasit = true;
			}
		}
		if (!gasit) {
			cout << "Nu a fost gasti niciun angajat cu numele: " << nume << endl;
		}
	}
};
int Departament::nr_total_angajati = 0;

int main() {
	cout <<endl<< "~~~~~~~~~~~~~~~~~ Testare clasa de baza ~~~~~~~~~~~~~~~~~~~~~~~" << endl;
	Persoana persoana1;
	cout << persoana1;
	Persoana persoana2("Mihai", 35);
	cout << endl << persoana2;
	persoana2.setvarsta(50);
	persoana2.setNume("Dan");
	cout << endl << "Persoana 2 are varsta de  " << persoana2.getvarsta()<<"ani";
	cout << endl << "Numele Persoanei 2 este  " << persoana2.getNume();
	Persoana persoana3;
	persoana3 = persoana2;
	cout << endl << persoana3;
	try {
		persoana1.getNume();
	}
		catch(const exception& e){
			cout << endl << endl << "A crapat aplicatia :  " << e.what() << endl;
	}
	cin >> persoana1;
	cout << persoana1;

	cout << endl << "~~~~~~~~~~~~~~~~~~ Sfarsit clasa de baza ~~~~~~~~~~~~~~~~~~~" << endl;
	cout << endl << "~~~~~~~~~~~~~~~~~~ Testare clasa mostenita Angajat ~~~~~~~~~~~~~~~~~~~" << endl<<endl;
	
	Angajat angajat0;
	cout << endl << angajat0;
	Angajat angajat1("Marian",24,"Daniel",10000,2);
	cout << angajat1<<endl;
	angajat0 = angajat1;
	cout <<endl<< angajat0;
	angajat1.setnume_manager("Marius");
	cout << endl << "Numele managerului este " << angajat1.getnume_manager()<<endl;
	try {
		angajat1.setsalariu(-1);
	}
	catch (const exception& ew) {
		cout << endl << "!!!!!!!!Atentie!!!!!!!! " << ew.what() << endl;
	}
	try {
		angajat0.getId_angajat();
	}
	catch(const exception& eww){
		cout << endl << "!!!!!!!!!!!!!!Atentie" << eww.what() << endl;
	}
	Angajat angajat2;
	angajat2 = angajat1;
	angajat2.setId_angajat(3);
	cout<<endl << angajat2;
	cin >> angajat1;
	cout << angajat1;
	try {
		Angajat angajat3("Ion",30,"Mihai",100,1);
		cout << angajat3;
	}
	catch (const exception& e) {
		cout << endl<<"Eroare : " << e.what();
	}

	cout<<endl<<endl<<"~~~~~~~~~~~~~~~~~~ Sfarsit testare clasa mostenita Angat ~~~~~~~~~~~~~~~~";
	cout << endl << endl << "~~~~~~~~~~~~~~~~~~ Testare clasa Programator ~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	Programator programator0;
	cout <<endl<< programator0;
	Programator programator1("Paul", 32, "Iancu", 32314, 4, "C++", fullstack);
	cout << endl << programator1;
	programator0 = programator1;
	cout << endl << programator1;
	programator1.setlimbajProgramareFolosit("SQL");
	cout << endl << "Programatorul 1 utilizeaza limbajul de programare " << programator1.getlimbajProgramareFolosit();
	try {
		programator0.setPost(backend);
	}
	catch (const exception& ewww) {
		cout <<endl<< endl << "!!!!!!!!!!!!!!!!!!ATENTIE" << ewww.what() << endl;
	}
	cout<<endl<<"Programator are postul "<<programator0.getpost();
	
	cout << endl<<endl << "~~~~~~~~~~~ Sfarsit testare clasa mostenita Programator~~~~~~~~~~~~~";
	
	cout << endl << endl << "~~~~~~~~~~~~~~ Testare clasa Departament ~~~~~~~~~~~~~~~~~~~";
	Departament dep("ITP");
	Angajat* ang1 = new Angajat("Daniel", 30, "Ion", 2300, 1);
	Angajat* ang2 = new Angajat("Maria", 28, "Ana", 4860, 2);
	Angajat* ang3 = new Angajat("Iancu", 35, "Dana", 3242, 3);
	Departament dep2("HR");
	Angajat* ang4 = new Angajat("Marius", 34, "Ionela", 2313, 4);
	dep.adaugaAngajat(ang1);
	dep.adaugaAngajat(ang2);
	dep.adaugaAngajat(ang3);
	dep2.adaugaAngajat(ang4);
	cout << dep;
	cout <<endl<< "Numarul total de angajati din toate departamente " << Departament::getNrTotalAngajati();
	cout << endl <<endl<< "Salariu total al departamentului este: " << dep.salariuTotal() <<" lei"<< endl;
	cin >> dep;
	cout << dep;
	
	cout << endl << endl << "~~~~~~~~~~~~~~~ Sfarsit testare clasa Departament ~~~~~~~~~~~~~~~~~";
	
	cout << endl << endl << "~~~~~~~~~~~~~~~~Testare subpucnte bonus~~~~~~~~~~~~~~~~";
	vector<Angajat*> angajati;
	angajati.push_back(new Angajat("Daniel", 30, "Ion", 2300, 1));
	angajati.push_back(new Angajat("Maria", 28, "Ana", 4860, 2));
	angajati.push_back(new Angajat("Iancu", 35, "Dana", 3242, 3));
	sort(angajati.begin(), angajati.end(), [](Angajat* a, Angajat* b) {
		return a->getsalariu() > b->getsalariu();
		});
	for (auto angajat : angajati) {
		cout << "~~~~~~~~~~~~~~~~~~~~" << endl;
		cout << *angajat << endl;
		cout << "~~~~~~~~~~~~~~~~~~~~" << endl;
	}
	cout << "Introduceti numele angajatului pe care doriti sa il cautati: ";
	char nume_cautat[100];
	cin >> nume_cautat;
	dep.cautaAngajatDupaNume(nume_cautat);
	
	cout << endl;
	return 0;
}
