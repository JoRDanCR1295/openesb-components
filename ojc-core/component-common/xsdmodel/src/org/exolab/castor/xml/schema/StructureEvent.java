package org.exolab.castor.xml.schema;

import java.util.EventObject;

public class StructureEvent extends EventObject {
    
    private Structure sourceStructure;
    
    private Structure childStructure;

    public StructureEvent(Structure source, Structure child) {
	super(source);
	this.sourceStructure = source;
	this.childStructure = child;
    }

    public Structure getSourceStructure() {
	return this.sourceStructure;
    }

    public Structure getChildStructure() {
	return this.childStructure;
    }
}

    
