

package it.imolinfo.jbi4corba.utils;


import java.io.Serializable;
import java.util.Properties;
import java.util.Vector;

/**
 * This Class is used as Holder for data used by the
 * WSDLDescriptor creation
 * @author lacquaviva
 */

public class DescInfo implements Serializable{
    
    
    public static final int GORB=0;
    public static final int SORB=1;
    public static final int CORB=2;
    /**
     * LocalizationType 
     */
    private String localizationType="";
    
    /**
     * Corba Localization Address
     */
    private String  address="";
    private int     ORBType=GORB;
    private boolean isValid=false;

    public boolean isValid() {
        return isValid;
    }
    
    private Vector<Object[]> properties;
   
    
     public int getORBType() {
        return ORBType;
    }

    public void setORBType(int ORBType) {
        this.ORBType = ORBType;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
        isValid=propertiesAreValid();
    }
    
    /**
     * @return String localization Tyep
     */
    public String getLocalizationType() {
        return localizationType;
    }


    
    /**
     *
     */
    public void setLocalizationType(String localizationType) {
        this.localizationType = localizationType;
         isValid=propertiesAreValid();
    }

    /**
     * 
     */
    public Vector<Object[]> getDataSource() {
        return properties;
    }

    /**
     * 
     */
    public Properties getProperties(){
        Properties p=new Properties();
        for(int i=0;i<properties.size();i++){
            p.setProperty(properties.get(i)[0].toString(),properties.get(i)[1].toString());
        }
        return p;
    }
    
    /**
     * 
     */
    public void setProperties(Vector<Object[]> properties) {
        this.properties = properties;
         isValid=propertiesAreValid();
    }
    
    /**
     *
     */
    public boolean isStateless(){
        if(localizationType.equals("")){
        return false;
        }
        return true;
        
    }
    
    /**
     * Validates current properties values.
     *
     * @return  <code>true</code> if and only if there are not properties with
     *          value but without name. The checks made ignore blank characters.
     */
    private boolean propertiesAreValid() {
        
       if(properties!=null){
           for(int i=0;i<properties.size();i++){
            String name = properties.get(i)[0].toString();
             String value = properties.get(i)[1].toString();
             
             if (isBlank( name) && !isBlank( value)) {
                return false;
             }
          return true;        
        }
       }
        return false;
        
    }
    
    
    /**
     * Checks if a <code>String</code> is empty (<code>""</code>) or made by
     * blank characters only (<code>'&#92;u0020'</code>).
     *
     * @param   s  the string to check.
     * @return  <code>true</code> if and only if <code>s</code> is
     *          <code>null</code> or contains only space characters.
     */
    private static boolean isBlank(final String s) {
        return (s == null) || (s.trim().length() == 0);
    }
    
    
}
