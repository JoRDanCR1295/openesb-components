package org.glassfish.openesb.databasebc;

import javax.management.MBeanException;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.OpenDataException;

import net.java.hulp.measure.Group;

import com.sun.jbi.eManager.provider.PerformanceMeasurement;

/**
 * Implementation of PerformanceMeasurement
 *
 * @author Sun Microsystems
 */

public class JDBCPerformanceMeasurement implements PerformanceMeasurement {

    public static final String JDBC_BC_SOURCE_FILTER = ".*databasebc.*";

    /** 
     * Retrieves the performance insturmentation measurement for the specified endpoint. 
     * @param endpoint The endpoint name qualified by service name.
     * @return An instance of TabularData which holds the performance instrumentation measurement for the specified endpoint.
     * @throws OpenDataException if there's an error related to the openmbean data.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public TabularData getPerformanceInstrumentationMeasurement(String endpoint) throws OpenDataException, MBeanException {
        Group gr = Group.createGroup();
        gr.addPattern(JDBC_BC_SOURCE_FILTER, endpoint, getPerfCatFilter());
        return gr.fetchData();
    }

    /** 
     * Resets the performance measurements on the endpoint.
     * @param endpoint The endpoint name qualified by service name.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public void clearPeformaceInstrumentationMeasurement (String endpoint) throws MBeanException {
        Group gr = Group.createGroup();
        gr.addPattern(JDBC_BC_SOURCE_FILTER, endpoint, getPerfCatFilter());
        gr.clearData();
    }

    private String getPerfCatFilter() {
        StringBuffer perfCatFilter = new StringBuffer();
        for (int i=0; i < JDBCBindingLifeCycle.JDBC_PERF_CATEGORIES.length; i++) {
            perfCatFilter.append(JDBCBindingLifeCycle.JDBC_PERF_CATEGORIES[i]);
            if (i < JDBCBindingLifeCycle.JDBC_PERF_CATEGORIES.length-1) {
                perfCatFilter.append('|');
            }
        }
        return perfCatFilter.toString();
    }
}
