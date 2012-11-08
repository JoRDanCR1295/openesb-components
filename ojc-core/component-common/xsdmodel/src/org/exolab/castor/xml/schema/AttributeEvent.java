
package org.exolab.castor.xml.schema;

import java.util.EventObject;

public class AttributeEvent extends EventObject {

	private Structure source;
	
	private String attributeName;
	
	private Object attributeOldValue;
	
	private Object attributeNewValue;
	
    public AttributeEvent(Structure src,
    							String attrName, 
    							Object attrOldValue, 
								Object attrNewValue) {
    	super(src);
    	this.source = src;
    	this.attributeName = attrName;
    	this.attributeOldValue = attrOldValue;
    	this.attributeNewValue = attrNewValue;
    }

    public String getAttributeName() {
    	return this.attributeName;
    }
    
    public Object getOldValue() {
    	return this.attributeOldValue;
    }
    
    public Object getNewValue() {
    	return this.attributeNewValue;
    }
    
}
