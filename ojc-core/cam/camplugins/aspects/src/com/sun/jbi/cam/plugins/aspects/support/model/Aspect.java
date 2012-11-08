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
 * @(#)Aspect.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author graj
 *
 */
public class Aspect implements Serializable {
	private static final long serialVersionUID = 1L;
	
    String id;
    ExchangeType exchangeType;
    AspectInput aspectInput;
    List<AspectOutput> outputList = new ArrayList<AspectOutput>();
    Map<Integer, AspectAdvice> orderToAdviceMap = new HashMap<Integer, AspectAdvice>();

    /**
     * 
     */
    public Aspect() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param id
     * @param exchangeType
     * @param aspectInput
     * @param outputList
     * @param orderToAdviceMap
     */
    public Aspect(String id, ExchangeType exchangeType, AspectInput aspectInput, List<AspectOutput> outputList, Map<Integer, AspectAdvice> orderToAdviceMap) {
        super();
        this.id = id;
        this.exchangeType = exchangeType;
        this.aspectInput = aspectInput;
        this.outputList = outputList;
        this.orderToAdviceMap = orderToAdviceMap;
    }


    /**
     * @return the id
     */
    public String getId() {
        return id;
    }


    /**
     * @param id the id to set
     */
    public void setId(String id) {
        this.id = id;
    }


    /**
     * @return the exchangeType
     */
    public ExchangeType getExchangeType() {
        return exchangeType;
    }

    /**
     * @param exchangeType the exchangeType to set
     */
    public void setExchangeType(ExchangeType exchangeType) {
        this.exchangeType = exchangeType;
    }

    /**
     * @return the aspectInput
     */
    public AspectInput getInput() {
        return aspectInput;
    }

    /**
     * @param aspectInput the aspectInput to set
     */
    public void setInput(AspectInput aspectInput) {
        this.aspectInput = aspectInput;
    }

    /**
     * @return the orderToAdviceMap
     */
    public Map<Integer, AspectAdvice> getOrderToAdviceMap() {
        return orderToAdviceMap;
    }

    /**
     * @param orderToAdviceMap the orderToAdviceMap to set
     */
    public void setOrderToAdviceMap(Map<Integer, AspectAdvice> orderToAdviceMap) {
        this.orderToAdviceMap = orderToAdviceMap;
    }
    
    public void addAdvice(AspectAdvice aspectAdvice) {
        this.orderToAdviceMap.put(aspectAdvice.getOrder(), aspectAdvice);
    }
    
    public void getAdvice(Integer order) {
        this.orderToAdviceMap.get(order);
    }
    
    public void removeAdvice(Integer order) {
        this.orderToAdviceMap.remove(order);
    }

    /**
     * @return the outputList
     */
    public List<AspectOutput> getOutputList() {
        return outputList;
    }

    /**
     * @param outputList the outputList to set
     */
    public void setOutputList(List<AspectOutput> outputList) {
        this.outputList = outputList;
    }
    
    public void addOutput(AspectOutput aspectOutput) {
        if(aspectOutput != null) {
            this.outputList.add(aspectOutput);
        }
    }
    
    public AspectAdvice[] retrieveAdvicesInOrder() {
        List<AspectAdvice> adviceList = new ArrayList<AspectAdvice>();
        
        Set<Integer> keySet = this.orderToAdviceMap.keySet();
        Integer[] keyArray = toArray(keySet, Integer.class);
        Arrays.sort(keyArray);
        for(int index = 0; index < keyArray.length; index++) {
            adviceList.add(this.orderToAdviceMap.get(keyArray[index]));
        }
        
        return toArray(adviceList, AspectAdvice.class);
    }
    
    /**
     * We can now use this to produce lists of ints or Strings:
     * List<Integer> ints = Lists.toList(1, 2, 3);
     * List<String> names = Lists.toList("Gopalan", "Suresh", "Raj");
     * 
     * @param array
     * @return
     */
    public static <T> List<T> toList(T... array) {
        List<T> list = new ArrayList<T>();
        for (T arrayElement : array) {
        	list.add(arrayElement);
        }
        return list;
    }
    
    /**
     * 
     * @param collection
     * @param componentType
     * @return
     */
    @SuppressWarnings("unchecked")
    static public <T> T[] toArray(Collection<T> collection,
            Class<T> componentType) {
        // unchecked cast
        T[] array = (T[]) java.lang.reflect.Array.newInstance(componentType,
                collection.size());
        int index = 0;
        for (T value : collection) {
            array[index++] = value;
        }
        return array;
    }
    

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
