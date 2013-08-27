#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * SUDescriptor.java
 */

package net.openesb.component.${componentName}.common.deployment;

import net.openesb.component.${componentName}.common.RuntimeHelper;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.jbi.management.DeploymentException;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * This interface represent the service unit descriptor (jbi.xml) model. This will be used in
 * ServiceUnit implementation to find the services provided and consumed by this service unit.
 * {@link SUDescriptorFactory${symbol_pound}getSUDescriptor} method reads the jbi.xml in the service unit to
 * to the implementation of this interface.
 *
 * @see SUDescriptorFactory
 * @see Consumes
 * @see Provides
 * @see ServiceUnit
 *
 * @author chikkala
 */
public interface SUDescriptor {
    
    Collection<Provides> getProvidedServices();
    
    Collection<Consumes> getConsumedServices();
    
    boolean isFor${componentName}();
    
    /**
     * base interface that models the service information described in the service unit descriptor for
     * consumed services and provided services.
     */
    public interface Service {
        /**
         * Getter for property interfaceQName.
         * @return Value of property interfaceQName.
         */
        QName getInterface();
        /**
         * Getter for property serviceName.
         * @return Value of property serviceName.
         */
        QName getServiceName();
        /**
         * Getter for property endpointName.
         * @return Value of property endpointName.
         */
        String getEndpointName();
    }
    /**
     * marker interface that represents the provided services in the service unit
     */
    public interface Provides extends Service {
    }
    /**
     * this interface represents the consumed service information in the su descriptor.
     */
    public interface Consumes extends Service {
        
        public final static String STANDARD_LINK = "standard";
        public final static String SOFT_LINK = "soft";
        public final static String HARD_LINK = "hard";
        /**
         * Getter for property linkType.
         * @return Value of property linkType.
         */
        String getLinkType();
    }
    /**
     * This is a factory class that can build the Service Unit Descriptor model from the jbi.xml
     */
    public static class SUDescriptorFactory {
        
        protected final static String JBI_TAG_NAME = "services";
        protected final static String SERVICES_TAG_NAME = "services";
        protected final static String BC_TAG_NAME = "binding-component";
        protected final static String PROVIDES_TAG_NAME = "provides";
        protected final static String CONSUMES_TAG_NAME = "consumes";
        protected final static String INTERFACE_TAG_NAME = "interface-name";
        protected final static String SERVICE_TAG_NAME = "service-name";
        protected final static String ENDPOINT_TAG_NAME = "endpoint-name";
        protected final static String LINK_TYPE_TAG_NAME = "link-type";
        /**
         * method that builds the Service unit descriptor model from the jbi.xml
         */
        public static SUDescriptor getSUDescriptor(String jbiXmlPath) throws Exception {
            FileReader reader = null;
            try {
                reader = new FileReader(jbiXmlPath);
                SUDescriptor suDesc = getSUDescriptor(reader);
                return suDesc;
            } finally {
                if ( reader != null ) {
                    try {
                        reader.close();
                    } catch (IOException ex) {
                        // ignore
                    }
                }
            }
        }
        /**
         * method that builds the Service unit descriptor model from the jbi.xml
         */
        public static SUDescriptor getSUDescriptor(Reader reader) throws Exception {
            SUDescriptor suDescriptor = null;
            Document suDescDoc = RuntimeHelper.buildDOMDocument(reader);
            Element jbiEl = suDescDoc.getDocumentElement();
            if (JBI_TAG_NAME.equals(jbiEl.getTagName())) {
                throw new DeploymentException("Invalid service unit descriptor : no jbi root element");
            }
            
            NodeList servicesNL = jbiEl.getElementsByTagName(SERVICES_TAG_NAME);
            if (servicesNL != null && servicesNL.getLength() == 1) {
                Element servicesEl = (Element) servicesNL.item(0);
                suDescriptor = SUDescriptorImpl.createSUDescriptor(servicesEl);
            } else {
                throw new DeploymentException("Invalid service unit descriptor : invalid services element");
            }
            
            return suDescriptor;
        }
        
    }
    /**
     * This class implements SUDescriptor
     */
    public static class SUDescriptorImpl implements SUDescriptor {
        
        private List<Consumes> mConsumedList;
        private List<Provides> mProvidedList;
        private boolean mIsFor${componentName};
        
        protected SUDescriptorImpl(boolean isFor${componentName}) {
            this.mIsFor${componentName} = isFor${componentName};
            this.mConsumedList = new ArrayList<Consumes>();
            this.mProvidedList = new ArrayList<Provides>();
        }
        
        protected void addProvidedService(Provides provides) {
            this.mProvidedList.add(provides);
        }
        
        protected void addConsumedService(Consumes consumes) {
            this.mConsumedList.add(consumes);
        }
        
        public Collection<Provides> getProvidedServices() {
            // return unmodifiable collection
            return Collections.unmodifiableCollection(this.mProvidedList);
        }
        
        public Collection<Consumes> getConsumedServices() {
            // return unmodifiable collection
            return Collections.unmodifiableCollection(this.mConsumedList);
        }
        
        public boolean isFor${componentName}() {
            return this.mIsFor${componentName};
        }
        
        protected static SUDescriptor createSUDescriptor(Element servicesEl) throws Exception {
            boolean isForBC = false;
            String bcTagString = servicesEl.getAttribute(SUDescriptorFactory.BC_TAG_NAME);
            isForBC = Boolean.valueOf(bcTagString).booleanValue();
            SUDescriptorImpl suDesc = new SUDescriptorImpl(isForBC);
            // add consumes
            NodeList consumesNL = servicesEl.getElementsByTagName(SUDescriptorFactory.CONSUMES_TAG_NAME);
            for ( int i=0; i < consumesNL.getLength(); ++i) {
                Element consumesEl = (Element) consumesNL.item(i);
                Consumes consumes = ConsumedService.createConsumedService(consumesEl);
                suDesc.addConsumedService(consumes);
            }
            // add provides
            NodeList providesNL = servicesEl.getElementsByTagName(SUDescriptorFactory.PROVIDES_TAG_NAME);
            for ( int i=0; i < providesNL.getLength(); ++i) {
                Element providesEl = (Element) providesNL.item(i);
                Provides provides = ProvidedService.createProvidedService(providesEl);
                suDesc.addProvidedService(provides);
            }
            
            return suDesc;
        }
    }
    /**
     * Base class that implements the Service interface
     */
    public static abstract class AbstractService implements Service {
        
        private QName mInterface;
        private QName mServiceName;
        private String mEndpointName;
        
        private AbstractService() {
        }
        /**
         * Getter for property interfaceQName.
         * @return Value of property interfaceQName.
         */
        public QName getInterface() {
            return this.mInterface;
        }
        
        /**
         * Setter for property interfaceQName.
         * @param interfaceQName New value of property interfaceQName.
         */
        protected void setInterface(QName interfaceQName) {
            this.mInterface = interfaceQName;
        }
        
        /**
         * Getter for property serviceName.
         * @return Value of property serviceName.
         */
        public QName getServiceName() {
            return this.mServiceName;
        }
        
        /**
         * Setter for property serviceName.
         * @param serviceName New value of property serviceName.
         */
        protected void setServiceName(QName serviceName) {
            this.mServiceName = serviceName;
        }
        
        /**
         * Getter for property endpointName.
         * @return Value of property endpointName.
         */
        public String getEndpointName() {
            return this.mEndpointName;
        }
        
        /**
         * Setter for property endpointName.
         * @param endpointName New value of property endpointName.
         */
        protected void setEndpointName(String endpointName) {
            this.mEndpointName = endpointName;
        }
        
    }
    /**
     * This class implements the Provides interface
     */
    public static class ProvidedService
        extends AbstractService
        implements Provides {
        protected ProvidedService(QName interfaceQName, QName serviceName, String endpointName) {
            this.setInterface(interfaceQName);
            this.setServiceName(serviceName);
            this.setEndpointName(endpointName);
        }
        
        @Override
        public String toString() {
            return "Provides :" +
                "${symbol_escape}n${symbol_escape}t interface-name= " + getInterface() +
                "${symbol_escape}n${symbol_escape}t service-name= " + getServiceName() +
                "${symbol_escape}n${symbol_escape}t  endpont-name= " + getEndpointName();
        }
        
        protected static Provides createProvidedService(Element providesEl) throws Exception {
            
            String ifName = providesEl.getAttribute(SUDescriptorFactory.INTERFACE_TAG_NAME);
            String serviceName = providesEl.getAttribute(SUDescriptorFactory.SERVICE_TAG_NAME);
            String endpointName = providesEl.getAttribute(SUDescriptorFactory.ENDPOINT_TAG_NAME);
            if ( ifName == null || serviceName == null || endpointName == null ) {
                throw new Exception("Invalid provides element: missing " + SUDescriptorFactory.INTERFACE_TAG_NAME +
                    " or " + SUDescriptorFactory.SERVICE_TAG_NAME + " or " + SUDescriptorFactory.ENDPOINT_TAG_NAME );
            }
            QName ifQName = RuntimeHelper.resolveAttrQName(ifName, providesEl);
            QName serviceQName = RuntimeHelper.resolveAttrQName(serviceName, providesEl);
            
            return new ProvidedService(ifQName, serviceQName, endpointName);
        }
    }
    /**
     * This class implements the Consumes interface.
     */
    public static class ConsumedService
        extends AbstractService
        implements Consumes {
        private String mLinkType;
        protected ConsumedService(QName interfaceQName,
            QName serviceName,  String endpointName, String linkType) {
            this.setInterface(interfaceQName);
            this.setServiceName(serviceName);
            this.setEndpointName(endpointName);
            this.mLinkType = linkType;
        }
        
        /**
         * Getter for property linkType.
         * @return Value of property linkType.
         */
        public String getLinkType() {
            return this.mLinkType;
        }
        
        @Override
        public String toString() {
            return "Cosumes :" +
                "${symbol_escape}n${symbol_escape}t interface-name= " + getInterface() +
                "${symbol_escape}n${symbol_escape}t service-name= " + getServiceName() +
                "${symbol_escape}n${symbol_escape}t  endpont-name= " + getEndpointName() +
                "${symbol_escape}n${symbol_escape}t  link-type= " + getLinkType();
        }
        
        protected static Consumes createConsumedService(Element consumesEl) throws Exception {
            
            String ifName = consumesEl.getAttribute(SUDescriptorFactory.INTERFACE_TAG_NAME);
            String serviceName = consumesEl.getAttribute(SUDescriptorFactory.SERVICE_TAG_NAME);
            String endpointName = consumesEl.getAttribute(SUDescriptorFactory.ENDPOINT_TAG_NAME);
            String linkType = consumesEl.getAttribute(SUDescriptorFactory.LINK_TYPE_TAG_NAME);
            if ( linkType == null || linkType.trim().length() == 0 ) {
                linkType = STANDARD_LINK;
            }
            
            if ( ifName == null ) {
                throw new Exception("Invalid consumes element: missing " +
                    SUDescriptorFactory.INTERFACE_TAG_NAME );
            }
            if ( serviceName == null || endpointName == null ) {
                throw new Exception("Invalid consumes element: missing " +
                    SUDescriptorFactory.SERVICE_TAG_NAME + " or "
                    + SUDescriptorFactory.ENDPOINT_TAG_NAME );
            }
            
            QName ifQName = RuntimeHelper.resolveAttrQName(ifName, consumesEl);
            QName serviceQName = null;
            if ( serviceName != null ) {
                serviceQName = RuntimeHelper.resolveAttrQName(serviceName, consumesEl);
            }
            if ( serviceQName != null && endpointName != null && linkType != null ) {
                if (!(STANDARD_LINK.equals(linkType) ||
                    SOFT_LINK.equals(linkType) || HARD_LINK.equals(linkType)) ) {
                    throw new Exception("Invalid consumes attribute value" +
                        SUDescriptorFactory.LINK_TYPE_TAG_NAME + "=" + linkType);
                }
            }
            return new ConsumedService(ifQName, serviceQName, endpointName, linkType);
        }
    }
    
}
