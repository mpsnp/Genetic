unit Utils;
{$mode objfpc}
interface

function min(a,b:longint):longint;
function max(a,b:longint):longint;

implementation

function min(a,b:longint):longint;
begin
	if b<a then
		result:=b
	else
		result:=a;
end;

function max(a,b:longint):longint;
begin
	if b>a then
		result:=b
	else
		result:=a;
end;

end.
