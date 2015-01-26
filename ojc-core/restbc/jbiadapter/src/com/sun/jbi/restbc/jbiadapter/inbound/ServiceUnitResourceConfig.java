package com.sun.jbi.restbc.jbiadapter.inbound;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ws.rs.Path;
import javax.ws.rs.ext.Provider;

import com.sun.jersey.api.core.DefaultResourceConfig;

/**
 * ServiceUnitResourceConfig.java
 *
 * @author Edward Chou
 */
public class ServiceUnitResourceConfig extends DefaultResourceConfig {
    
    public static final String PROPERTY_CLASSPATH
            = "com.sun.jersey.config.property.classpath";
    
    public static final String SERVICE_UNIT_ROOT = 
        "com.sun.jbi.config.property.serviceunitroot";
    
    private static final Logger LOGGER = 
            Logger.getLogger(ServiceUnitResourceConfig.class.getName());

    private final String[] paths;
    
    private ClassLoader serviceUnitClassLoader;
    
    public ServiceUnitResourceConfig() {
        this(getPaths(), null);
    }
    
    public ServiceUnitResourceConfig(Map<String, Object> props) {
        this(getPaths(props), props);
        
        setPropertiesAndFeatures(props);
    }

    public ServiceUnitResourceConfig(String[] paths, Map<String, Object> props) {
        super();
        
        if (paths == null || paths.length == 0)
            throw new IllegalArgumentException(
                    "Array of paths must not be null or empty");

        this.paths = paths.clone();
        
        try {
            Object v = props.get(SERVICE_UNIT_ROOT);
            if (v == null)
                throw new IllegalArgumentException(SERVICE_UNIT_ROOT + " property is missing");

            String serviceUnitRoot = (String) v;
            URL serviceUnitRootURL = new File(serviceUnitRoot).toURL();
            serviceUnitClassLoader = new URLClassLoader(new URL[] { serviceUnitRootURL }, this.getClass().getClassLoader());
        } catch(Exception e) {
            LOGGER.log(Level.WARNING, "unable to create ServiceUnit ClassLoader", e);
        }
        
        init(paths);
    }
    
    /**
     * Perform a new search for resource classes and provider classes.
     */
    public void reload() {
        getClasses().clear();
        init(paths);
    }
        
    private void init(String[] paths) {    
        File[] roots = new File[paths.length];
        for (int i = 0;  i< paths.length; i++) {
            roots[i] = new File(paths[i]);
        }

        if (LOGGER.isLoggable(Level.INFO)) {
            StringBuilder b = new StringBuilder();
            b.append("Scanning for root resource and provider classes in the paths:");
            for (String p : paths)
                b.append('\n').append("  ").append(p);
            
            LOGGER.log(Level.INFO, b.toString());            
        }
        
        
        JaxrsAnnotationScanner scanner = new JaxrsAnnotationScanner(serviceUnitClassLoader, Path.class, Provider.class);
        
        scanner.scan(roots);

        getClasses().addAll(scanner.getMatchingClasses());
        
        if (LOGGER.isLoggable(Level.INFO) && !getClasses().isEmpty()) {
            StringBuilder b = new StringBuilder();
            b.append("Root resource classes found:");
            for (Class c : getClasses()) {
                if (c.isAnnotationPresent(Path.class)) {
                    b.append('\n').append("  ").append(c);
                }
            }
            
            LOGGER.log(Level.INFO, b.toString());
            
            b = new StringBuilder();
            b.append("Provider classes found:");
            for (Class c : getClasses()) {
                if (c.isAnnotationPresent(Provider.class)) {
                    b.append('\n').append("  ").append(c);
                }
            }
            
            LOGGER.log(Level.INFO, b.toString());            
        }
    }
    
    private static String[] getPaths() {
        String classPath = System.getProperty("java.class.path");
        return classPath.split(File.pathSeparator);                
    }
    
    private static String[] getPaths(Map<String, Object> props) {
        Object v = props.get(PROPERTY_CLASSPATH);
        if (v == null)
            throw new IllegalArgumentException(PROPERTY_CLASSPATH + 
                    " property is missing");
        
        String[] paths = getPaths(v);
        if (paths.length == 0)
            throw new IllegalArgumentException(PROPERTY_CLASSPATH + 
                    " contains no paths");
        
        return paths;
    }
    
    private static String[] getPaths(Object param) {
        if (param instanceof String) {
            return getElements(new String[] { (String)param });
        } else if (param instanceof String[]) {
            return getElements((String[])param);
        } else {
            throw new IllegalArgumentException(PROPERTY_CLASSPATH + " must " +
                    "have a property value of type String or String[]");
        }
    }
    
    /* (non-Javadoc)
     * @see com.sun.jersey.api.core.DefaultResourceConfig#getClasses()
     */
    @Override
    public Set<Class<?>> getClasses() {
        Set<Class<?>> classes = super.getClasses();
        classes.add(BundleContextProvider.class);
        return classes;
    }

}
