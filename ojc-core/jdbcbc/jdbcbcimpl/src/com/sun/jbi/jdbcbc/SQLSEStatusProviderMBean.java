/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.jdbcbc;


import javax.management.openmbean.TabularData;
import javax.management.InvalidAttributeValueException;
import javax.management.openmbean.OpenDataException;
import javax.management.MBeanException;

/**
 *
 * @author narayan
 */
public interface SQLSEStatusProviderMBean {
    
     /**
     * Gets the performance instrumenation measurement categories supported by a component.
     * @return An array of String - each entry in the array specifies the category of measurement contained in the performance instrumenation measurement TabularData returned by the component.
     */
    public String[] getPerformanceMeasurementCategories();
        
    
    /** 
     * Retrieves the performance insturmentation measurement for the specified endpoint. 
     * @param endpoint The endpoint name qualified by service name.
     * @return An instance of TabularData which holds the performance instrumentation measurement for the specified endpoint.
     * @throws OpenDataException if there's an error related to the openmbean data.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public TabularData getPerformanceInstrumentationMeasurement(String endpoint) throws OpenDataException, MBeanException;
    
    
    /** 
     * Resets the performance measurements on the endpoint.
     * @param endpoint The endpoint name qualified by service name.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public void clearPeformaceInstrumentationMeasurement (String endpoint) throws MBeanException;

     /** 
    * Retrieves the main WSDL associated with the specified endpoint. 
    * @param endpoint The endpoint name qualified by service name.
    * @return The content of the main WSDL associated with the given endpoint.
    * @throws MBeanException if the endpoint does not exist for the 
    */
    public String getWSDLDefinition(String endpoint) throws MBeanException;
    
    /** 
    * Retrieves the imported WSDL or XSD associated with the specified endpoint. 
    * @param endpoint The endpoint name qualified by service name.
    * @return The content of the imported resource, WSDL or XSD associated with the given endpoint.
    */
    public String getWSDLImportedResource(String endpoint, String namespace)  throws MBeanException;
    
}
