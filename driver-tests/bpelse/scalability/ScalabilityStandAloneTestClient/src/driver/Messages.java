/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)EngineChannel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package driver;

import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import com.sun.java.products.oss.xml.common.ApplicationContext;
import com.sun.java.products.oss.xml.common.ArrayOfString;
import com.sun.java.products.oss.xml.common.ArrayOfSystemProperties;
import com.sun.java.products.oss.xml.common.Modifier;
import com.sun.java.products.oss.xml.common.SystemProperty;
import com.sun.java.products.oss.xml.troubleticket.ActivityDuration;
import com.sun.java.products.oss.xml.troubleticket.ActivityDurationList;
import com.sun.java.products.oss.xml.troubleticket.Address;
import com.sun.java.products.oss.xml.troubleticket.ArrayOfActivityDuration;
import com.sun.java.products.oss.xml.troubleticket.ArrayOfAuthorization;
import com.sun.java.products.oss.xml.troubleticket.ArrayOfCustomerRoleAssignment;
import com.sun.java.products.oss.xml.troubleticket.ArrayOfEscalation;
import com.sun.java.products.oss.xml.troubleticket.Authorization;
import com.sun.java.products.oss.xml.troubleticket.CreateTroubleTicketByValueRequest;
import com.sun.java.products.oss.xml.troubleticket.CustomerRoleAssignment;
import com.sun.java.products.oss.xml.troubleticket.Escalation;
import com.sun.java.products.oss.xml.troubleticket.EscalationList;
import com.sun.java.products.oss.xml.troubleticket.OrgLevel;
import com.sun.java.products.oss.xml.troubleticket.PersonReach;
import com.sun.java.products.oss.xml.troubleticket.RequestState;
import com.sun.java.products.oss.xml.troubleticket.TroubleTicketKey;
import com.sun.java.products.oss.xml.troubleticket.TroubleTicketValue;

/**
 *
 * @author mbhasin
 */
public class Messages {

    public Messages() {
    }

    public static CreateTroubleTicketByValueRequest getCreateTroubleTicketByValueRequest() throws Exception {
        CreateTroubleTicketByValueRequest createByValue = new CreateTroubleTicketByValueRequest();
        TroubleTicketValue ttValue = new TroubleTicketValue();

        JAXBElement<TroubleTicketKey> troubleTicketKey = getTroubleTicketKey();
        // @XmlElementRef(name = "accountOwner", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        PersonReach person = getPersonReach();
        JAXBElement<PersonReach> accountOwner = new javax.xml.bind.JAXBElement<PersonReach>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "accountOwner"), PersonReach.class, person);

        JAXBElement<ActivityDurationList> activityDurationListElem = getActivityDurationList();

        // @XmlElementRef(name = "additionalTroubleInfoList", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        ArrayOfString aos = new ArrayOfString();
        java.util.List<String> ls = aos.getItem();
        ls.add("customer");
        ls.add("troubleTicketKey");
        ls.add("troubleDescription");
        ls.add("troubleFound");
        ls.add("troubleType");

        JAXBElement<ArrayOfString> additionalTroubleInfoList = new JAXBElement<ArrayOfString>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "additionalTroubleInfoList"),
                ArrayOfString.class, aos);

        // @XmlElementRef(name = "afterHoursRepairAuthority", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<Boolean> afterHoursRepairAuthority = new JAXBElement<Boolean>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "afterHoursRepairAuthority"), Boolean.class,
                false);

        ArrayOfAuthorization aoa = getArrayOfAuthorization();
        JAXBElement<ArrayOfAuthorization> authorizationList = new JAXBElement<ArrayOfAuthorization>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "authorizationList"),
                ArrayOfAuthorization.class, aoa);

        // @XmlElementRef(name = "cancelRequestedByCustomer", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<Boolean> cancelRequestedByCustomer = new JAXBElement<Boolean>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "cancelRequestedByCustomer"), Boolean.class,
                false);

        // @XmlElementRef(name = "clearancePerson", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        PersonReach personReach2 = getPersonReach();
        ;
        JAXBElement<PersonReach> clearancePerson = new JAXBElement<PersonReach>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "clearancePerson"), PersonReach.class,
                personReach2);

        // @XmlElementRef(name = "closeOutNarr", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> closeOutNarr = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "closeOutNarr"), String.class, "closeOutNarr");

        // @XmlElementRef(name = "baseCloseOutVerification", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> baseCloseOutVerification = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "baseCloseOutVerification"), String.class,
                "baseCloseOutVerification");

        // @XmlElementRef(name = "commitmentTime", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        XMLGregorianCalendar commitmentTimeCal = getXMLGregorianCalendar("2016-01-01T11:07:42");
        JAXBElement<XMLGregorianCalendar> commitmentTime = new JAXBElement<XMLGregorianCalendar>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "commitmentTime"),
                XMLGregorianCalendar.class, commitmentTimeCal);

        // @XmlElementRef(name = "commitmentTimeRequested", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        XMLGregorianCalendar commitmentTimeRequestedCal = getXMLGregorianCalendar("2016-01-01T11:07:42");
        JAXBElement<XMLGregorianCalendar> commitmentTimeRequested = new JAXBElement<XMLGregorianCalendar>(
                new QName("http://java.sun.com/products/oss/xml/TroubleTicket", "commitmentTimeRequested"),
                XMLGregorianCalendar.class, commitmentTimeRequestedCal);

        // @XmlElementRef(name = "customer", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        PersonReach personReach3 = getPersonReach();
        JAXBElement<PersonReach> customer = new JAXBElement<PersonReach>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "customer"), PersonReach.class, personReach3);

        // @XmlElementRef(name = "customerRoleAssignmentList", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        ArrayOfCustomerRoleAssignment aocra = getArrayOfCustomerRoleAssignment();

        JAXBElement<ArrayOfCustomerRoleAssignment> customerRoleAssignmentList = new JAXBElement<ArrayOfCustomerRoleAssignment>(
                new QName("http://java.sun.com/products/oss/xml/TroubleTicket", "customerRoleAssignmentList"),
                ArrayOfCustomerRoleAssignment.class, aocra);

        // @XmlElementRef(name = "customerTroubleNum", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> customerTroubleNum = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "customerTroubleNum"), String.class,
                "customerTroubleNum");

        // @XmlElementRef(name = "dialog", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> dialog = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "dialog"), String.class, "dialog");

        // @XmlElementRef(name = "escalationList", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        EscalationList escalationList1 = getEscalationList();
        JAXBElement<EscalationList> escalationList = new JAXBElement<EscalationList>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "escalationList"), EscalationList.class,
                escalationList1);

        // @XmlElementRef(name = "baseInitiatingMode", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> baseInitiatingMode = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "baseInitiatingMode"), String.class,
                "baseInitiatingMode");

        // @XmlElementRef(name = "lastUpdateTime", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        XMLGregorianCalendar lastUpdateTimeCal = getXMLGregorianCalendar("2016-01-01T11:07:42");
        JAXBElement<XMLGregorianCalendar> lastUpdateTime = new JAXBElement<XMLGregorianCalendar>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "lastUpdateTime"),
                XMLGregorianCalendar.class, lastUpdateTimeCal);

        // @XmlElementRef(name = "maintServiceCharge", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<Boolean> maintServiceCharge = new JAXBElement<Boolean>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "maintServiceCharge"), Boolean.class, false);

        ttValue.setTroubleTicketKey(troubleTicketKey);
        ttValue.setAccountOwner(accountOwner);
        ttValue.setActivityDurationList(activityDurationListElem);
        ttValue.setAdditionalTroubleInfoList(additionalTroubleInfoList);
        ttValue.setAfterHoursRepairAuthority(afterHoursRepairAuthority);
        ttValue.setAuthorizationList(authorizationList);
        ttValue.setCancelRequestedByCustomer(cancelRequestedByCustomer);
        ttValue.setClearancePerson(clearancePerson);
        ttValue.setCloseOutNarr(closeOutNarr);
        ttValue.setBaseCloseOutVerification(baseCloseOutVerification);
        ttValue.setCommitmentTime(commitmentTime);
        ttValue.setCommitmentTimeRequested(commitmentTimeRequested);
        ttValue.setCustomer(customer);
        ttValue.setCustomerRoleAssignmentList(customerRoleAssignmentList);
        ttValue.setCustomerTroubleNum(customerTroubleNum);
        ttValue.setDialog(dialog);
        ttValue.setEscalationList(escalationList);
        ttValue.setBaseInitiatingMode(baseInitiatingMode);
        ttValue.setLastUpdateTime(lastUpdateTime);
        ttValue.setMaintServiceCharge(maintServiceCharge);

        createByValue.setTroubleTicketValue(ttValue);
        return createByValue;
    }
    
    private static JAXBElement<TroubleTicketKey> getTroubleTicketKey() {
        TroubleTicketKey ttKey = new TroubleTicketKey();
        ApplicationContext context = new ApplicationContext();
        context.setFactoryClass("factory class1");
        context.setUrl("http://www.any.com/verrantque/temperat");

        ArrayOfSystemProperties aosp = new ArrayOfSystemProperties();
        List<SystemProperty> lsSysProp = aosp.getProperty();
        SystemProperty property = new SystemProperty();
        property.setName("name 1");
        property.setValue("value 1");

        lsSysProp.add(property);
        context.setSystemProperties(aosp);

        ttKey.setApplicationContext(context);

        ttKey.setPrimaryKey("primary key 1");
        ttKey.setType("type1");
        ttKey.setApplicationDN("application dn 1");

        // @XmlElementRef(name = "troubleTicketKey", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<TroubleTicketKey> troubleTicketKey = new javax.xml.bind.JAXBElement<TroubleTicketKey>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "troubleTicketKey"), TroubleTicketKey.class,
                ttKey);
        return troubleTicketKey;

    }

    private static JAXBElement<ActivityDurationList> getActivityDurationList() {
        // @XmlElementRef(name = "activityDurationList", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        // activityDurationList.set
        ActivityDurationList activityDurationList = new ActivityDurationList();
        Modifier modifier = Modifier.NONE;
        activityDurationList.setModifier(modifier);

        ArrayOfActivityDuration aoad = new ArrayOfActivityDuration();
        java.util.List<ActivityDuration> ls = aoad.getItem();
        ActivityDuration duration = new ActivityDuration();

        JAXBElement<String> baseActivityType = getBaseActivityType("baseActivityType");
        // @XmlElementRef(name = "billable", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<Boolean> billable = new JAXBElement<Boolean>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "billable"), Boolean.class, false);

        duration.setBaseActivityType(baseActivityType);
        duration.setBillable(billable);
        ls.add(duration);

        // @XmlElementRef(name = "activityDurations", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<ArrayOfActivityDuration> activityDurations = new javax.xml.bind.JAXBElement<ArrayOfActivityDuration>(
                new QName("http://java.sun.com/products/oss/xml/TroubleTicket", "activityDurations"),
                ArrayOfActivityDuration.class, aoad);
        ;
        activityDurationList.setActivityDurations(activityDurations);

        JAXBElement<ActivityDurationList> activityDurationListElem = new javax.xml.bind.JAXBElement<ActivityDurationList>(
                new QName("http://java.sun.com/products/oss/xml/TroubleTicket", "activityDurationList"),
                ActivityDurationList.class, activityDurationList);
        return activityDurationListElem;

    }

    private static PersonReach getPersonReach() {

        // @XmlElementRef(name = "email", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> email = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "email"), String.class, "email");
        // @XmlElementRef(name = "fax", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> fax = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "fax"), String.class, "fax");

        // @XmlElementRef(name = "addressInfo", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        Address address = new Address();
        JAXBElement<String> addressInfo = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "addressInfo"), String.class, "addressInfo1");
        address.setAddressInfo(addressInfo);
        // @XmlElementRef(name = "location", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<Address> location = new JAXBElement<Address>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "location"), Address.class, address);
        // @XmlElementRef(name = "name", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> name = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "name"), String.class, "name");
        // @XmlElementRef(name = "number", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> number = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "number"), String.class, "number");
        // @XmlElementRef(name = "organizationName", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> organizationName = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "organizationName"), String.class,
                "organizationName");
        // @XmlElementRef(name = "phone", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> phone = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "phone"), String.class, "phone");
        // @XmlElementRef(name = "responsible", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> responsible = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "responsible"), String.class, "responsible");
        // @XmlElementRef(name = "sMSAddress", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> smsAddress = new JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "sMSAddress"), String.class, "sMSAddress");

        PersonReach person = new PersonReach();
        person.setEmail(email);
        person.setFax(fax);
        person.setLocation(location);
        person.setName(name);
        person.setNumber(number);
        person.setOrganizationName(organizationName);
        person.setPhone(phone);
        person.setResponsible(responsible);
        person.setSMSAddress(smsAddress);
        return person;
    }

    private static ArrayOfAuthorization getArrayOfAuthorization() throws Exception {
        // @XmlElementRef(name = "authorizationList", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        ArrayOfAuthorization aoa = new ArrayOfAuthorization();
        List<Authorization> ls2 = aoa.getItem();

        // @XmlElementRef(name = "baseActivityType", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> baseActivityType = getBaseActivityType("baseActivityType");

        // @XmlElementRef(name = "authPerson", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        PersonReach person = getPersonReach();
        JAXBElement<PersonReach> authPerson = new javax.xml.bind.JAXBElement<PersonReach>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "authPerson"), PersonReach.class, person);

        // @XmlElementRef(name = "authTime", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        XMLGregorianCalendar authTimeCal = getXMLGregorianCalendar("2016-01-01T11:07:42");
        JAXBElement<XMLGregorianCalendar> authTime = new JAXBElement<XMLGregorianCalendar>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "authTime"), XMLGregorianCalendar.class,
                authTimeCal);

        // @XmlElementRef(name = "requestState", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        RequestState requestStateVal = RequestState.REQUESTED;
        JAXBElement<RequestState> requestState = new JAXBElement<RequestState>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "requestState"), RequestState.class,
                requestStateVal);

        Authorization auth1 = new Authorization();

        auth1.setBaseActivityType(baseActivityType);
        auth1.setAuthPerson(authPerson);
        auth1.setAuthTime(authTime);
        auth1.setRequestState(requestState);
        ls2.add(auth1);
        return aoa;
    }

    private static JAXBElement<String> getBaseActivityType(String actType) {
        // @XmlElementRef(name = "baseActivityType", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> baseActivityType = new javax.xml.bind.JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "baseActivityType"), String.class, actType);
        return baseActivityType;
    }

    private static JAXBElement<String> getBaseRole(String role) {
        // @XmlElementRef(name = "baseRole", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> baseRole = new javax.xml.bind.JAXBElement<String>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "baseRole"), String.class, role);
        return baseRole;
    }

    private static XMLGregorianCalendar getXMLGregorianCalendar(String lexicalValue) throws Exception {
        XMLGregorianCalendar cal = DatatypeFactory.newInstance().newXMLGregorianCalendar(lexicalValue);
        return cal;
    }

    private static ArrayOfCustomerRoleAssignment getArrayOfCustomerRoleAssignment() {
        ArrayOfCustomerRoleAssignment aocra = new ArrayOfCustomerRoleAssignment();
        List<CustomerRoleAssignment> ls3 = aocra.getItem();
        CustomerRoleAssignment cra1 = new CustomerRoleAssignment();

        // @XmlElementRef(name = "baseRole", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        JAXBElement<String> baseRole = getBaseRole("base role");
        // @XmlElementRef(name = "contactPerson", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        PersonReach person = getPersonReach();
        JAXBElement<PersonReach> contactPerson = new JAXBElement<PersonReach>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "contactPerson"), PersonReach.class, person);
        ;

        cra1.setBaseRole(baseRole);
        cra1.setContactPerson(contactPerson);
        ls3.add(cra1);
        return aocra;
    }

    private static EscalationList getEscalationList() throws Exception {
        EscalationList escalationList1 = new EscalationList();
        escalationList1.setModifier(Modifier.NONE);

        ArrayOfEscalation aoe = new ArrayOfEscalation();
        List<Escalation> ls = aoe.getItem();
        Escalation escalation = new Escalation();

        // @XmlElementRef(name = "escPerson", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        PersonReach person = getPersonReach();
        JAXBElement<PersonReach> escPerson = new JAXBElement<PersonReach>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "escPerson"), PersonReach.class, person);

        // @XmlElementRef(name = "escTime", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        XMLGregorianCalendar escTimeCal = getXMLGregorianCalendar("2016-01-01T11:07:42");
        JAXBElement<XMLGregorianCalendar> escTime = new JAXBElement<XMLGregorianCalendar>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "escTime"), XMLGregorianCalendar.class,
                escTimeCal);

        // @XmlElementRef(name = "orgLevel", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        OrgLevel orgLevell = OrgLevel.NOESCALATION;
        JAXBElement<OrgLevel> orgLevel = new JAXBElement<OrgLevel>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "orgLevel"), OrgLevel.class, orgLevell);

        // @XmlElementRef(name = "requestPerson", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        PersonReach requestPersonn = getPersonReach();
        JAXBElement<PersonReach> requestPerson = new JAXBElement<PersonReach>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "requestPerson"), PersonReach.class,
                requestPersonn);

        // @XmlElementRef(name = "requestState", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)
        RequestState reqState = RequestState.NOSTATE;
        JAXBElement<RequestState> requestState = new JAXBElement<RequestState>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "requestState"), RequestState.class, reqState);

        escalation.setEscPerson(escPerson);
        escalation.setEscTime(escTime);
        escalation.setOrgLevel(orgLevel);
        escalation.setRequestPerson(requestPerson);
        escalation.setRequestState(requestState);
        ls.add(escalation);
        // @XmlElementRef(name = "escalations", namespace =
        // "http://java.sun.com/products/oss/xml/TroubleTicket", type = JAXBElement.class)

        JAXBElement<ArrayOfEscalation> escalations = new JAXBElement<ArrayOfEscalation>(new QName(
                "http://java.sun.com/products/oss/xml/TroubleTicket", "escalations"), ArrayOfEscalation.class, aoe);
        escalationList1.setEscalations(escalations);

        return escalationList1;
    }
}
