package org.eclipse.wazaabi.demo.contacts.database;

import org.eclipse.wazaabi.demo.contacts.model.contact.Contact;
import org.eclipse.wazaabi.demo.contacts.model.contact.ContactFactory;
import org.eclipse.wazaabi.demo.contacts.model.contact.Database;
import org.eclipse.wazaabi.demo.contacts.model.contact.Gender;

public class DataService {

	private final Database database = ContactFactory.eINSTANCE.createDatabase();

	private final Contact contacts[] = new Contact[] {
			createContact("Angelina", "Jolie", Gender.FEMALE,
					"Eclipse Foundation"),
			createContact("Brad", "Pitt", Gender.MALE, "Eclipse Foundation"),
			createContact("Denzel", "Washington", Gender.MALE, "IBM"),
			createContact("Jackie", "Chan", Gender.MALE, "CISCO"),
			createContact("Julia", "Roberts", Gender.FEMALE, "Oracle") };

	public DataService() {
		for (Contact contact : contacts)
			database.getContacts().add(contact);
	}

	public Database getDatabase() {
		return database;
	}

	public Contact createContact(String firstName, String lastName,
			Gender gender, String company) {
		Contact contact = ContactFactory.eINSTANCE.createContact();
		contact.setFirstName(firstName);
		contact.setLastName(lastName);
		contact.setGender(gender);
		contact.setCompany(company);
		return contact;
	}

	public void AddContact(String firstName, String lastName, Gender gender,
			String company) {
		getDatabase().getContacts().add(
				createContact(firstName, lastName, gender, company));
	}

	public void removeContact(Contact contact) {
		getDatabase().getContacts().remove(contact);
	}
}
