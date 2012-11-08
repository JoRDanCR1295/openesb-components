package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import java.util.GregorianCalendar;
import java.util.StringTokenizer;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;

import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;

public class DateTimeLessThan implements Function {

	public Object invoke(ExpressionContext arg0, Object[] args) {
    	if (args.length < 2) {
    		throw new RuntimeException("Imporper arguments supplied. Please pass two xsd dateTime values as arguments to" +
    				"dateTime-less-than() function");
    	}
    	
    	String first;
    	String second;
    	
    	if (args[0] instanceof ExpressionContext) {
    		first = (String)((ExpressionContext)args[0]).getContextNodePointer().getValue();
    	} else {
    		first = (String)args[0];
    	}
    	
    	if (args[1] instanceof ExpressionContext) {
    		second = (String)((ExpressionContext)args[1]).getContextNodePointer().getValue();
    	} else {
    		second = (String)args[1];
    	}
    	
		GregorianCalendar cal1 = getGregorianCalendar(first);
		GregorianCalendar cal2 = getGregorianCalendar(second);
		
		
		int result = cal1.compareTo(cal2);
		if (result < 0) {
			return new Boolean(true);
		} else {
			return new Boolean(false);
		}
	}
	
    public static GregorianCalendar getGregorianCalendar(String isodate) throws StandardException {
        StringTokenizer st = new StringTokenizer(isodate, DateParserImpl.DELIMITER, true);
        GregorianCalendar calendar = new GregorianCalendar(DateParserImpl.UTC);
        calendar.clear();
        try {
			DateParserImpl.getCalendar(st, calendar, isodate);
		} catch (RuntimeException e) {
			new StandardException(StandardException.Fault.InvalidVariables, e);
		}
        return calendar;
    }
}
