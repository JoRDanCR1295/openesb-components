package net.openesb.jbi.restbc.jbiadapter.descriptor;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents a service unit descriptor entry for <provides>, for example for
 * 
 * <code>
 *   <filter-chain xmlns="http://www.sun.com/jbi/restbc/jaxrs_filters">
 *     <filter classname="myjaxrsfilterproject.MyServerRequestFilter" name="MyServerRequestFilter">
 *       <init-properties>
 *         <property name="password" value="bar"/>
 *         <property name="userName" value="foo"/>
 *       </init-properties>
 *     </filter>
 *     <filter classname="myjaxrsfilterproject.MyServerResponseFilter" name="MyServerResponseFilter"/>
 *   </filter-chain>
 * </code>
 *
 * @author Edward Chou
 */
public class Filter {

    private String className;
    private String name;
    private Map<String, String> props = new HashMap<String, String> ();
    
    public Filter() {
    }
    
    /**
     * @return the className
     */
    public String getClassName() {
        return className;
    }

    /**
     * @param className the className to set
     */
    public void setClassName(String className) {
        this.className = className;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the props
     */
    public Map<String, String> getProps() {
        return Collections.unmodifiableMap(props);
    }

    /**
     * 
     * @param key
     * @param val
     */
    public void addProps(String key, String val) {
       props.put(key, val);
    }
    
}
