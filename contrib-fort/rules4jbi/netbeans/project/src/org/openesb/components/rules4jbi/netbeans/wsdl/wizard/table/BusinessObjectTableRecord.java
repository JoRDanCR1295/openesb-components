/*
 * @(#)BusinessObjectTableRecord.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class BusinessObjectTableRecord {

    private String type;
    
    private Direction direction;
    
    private Cardinality cardinality;

    public BusinessObjectTableRecord() {
        this("", new Direction(), new Cardinality());
    }
    
    public BusinessObjectTableRecord(String type, Direction direction, Cardinality cardinality) {
        if (type == null) {
            throw new NullPointerException("Type must not be null");
        }
        
        if (direction == null) {
            throw new NullPointerException("Direction must not be null");
        }
        
        if (cardinality == null) {
            throw new NullPointerException("Cardinality must not be null");
        }
        
        this.type = type;
        this.direction = direction;
        this.cardinality = cardinality;
    }

    public Cardinality getCardinality() {
        return cardinality;
    }

    public void setCardinality(Cardinality cardinality) {
        this.cardinality = cardinality;
    }

    public Direction getDirection() {
        return direction;
    }

    public void setDirection(Direction direction) {
        this.direction = direction;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        final String space = "    ";
        
        sb.append("Record");
        sb.append("\n");
        
        sb.append(space);
        sb.append("Type: ");
        sb.append(type);
        sb.append("\n");

        sb.append(space);
        sb.append("Direction: ");
        sb.append(direction.toDescriptiveString());
        sb.append("\n");

        sb.append(space);
        sb.append("Cardinality: ");
        sb.append(cardinality.toDescriptiveString());
        sb.append("\n");
        
        return sb.toString();
    }
    
//    public static void main(String[] args) {
//        BusinessObjectTableRecord record =
//                new BusinessObjectTableRecord("org.example.Customer", new Direction("IN"), new Cardinality("6"));
//        
//        System.out.println(record);
//    }
}
