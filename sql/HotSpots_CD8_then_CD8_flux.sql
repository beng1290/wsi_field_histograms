use test
go

set statistics time on
go

IF OBJECT_ID(N'fGetHotspotsE', N'TF') IS NOT NULL
    DROP Function dbo.fGetHotspotsE
GO
--
CREATE FUNCTION dbo.fGetHotspotsE(@tlo float, @thi float)
--------------------------------------------------------------------
-- dbo.fMarkerDensities(@N,@M)
-- 
-- Compute a table of hotspots based on the CD8 counts 
-- and the ordered ranks by descreasing density of CD8,
-- followed by decreasing density of all cells
--------------------------------------------------------------------
RETURNS @out table (
	sampleid int not null,
	hpfid int not null,
	area float not null,
	cd8count int not null,
	cd8_d float not null,
	cellcount float not null,
	cell_d float not null,
	r int not null
	)
AS BEGIN
	-----------------------------------------------------------
	-- get the fields with 
	-- annotation greater than 25%, and reduced 
	-- tumor fraction within @tlo and @thi
	-----------------------------------------------------------
	declare @t1 table(
		sampleid int not null,
		hpfid int not null,
		area float not null)
	insert into @t1
	select f.sampleid, f.hpfid, primarea area -- * annofrac area 
	from wsi.dbo.RTFieldGeometry f
	where f.tumorfrac between @tlo and @thi
	and f.annofrac >= .25
	group by f.sampleid, f.hpfid, primarea --* annofrac
	-----------------------------------------------------------
	-- get the cd8 counts of all fields in @t1
	-----------------------------------------------------------
	declare @t2 table(
		hpfid int not null,
		cd8count int not null
		)
	--
	insert into @t2
	select a.hpfid, count(*) cd8count
	from @t1 b, wsi.dbo.CellTag a 
	where a.ptype=2
	and a.hpfid = b.hpfid
	and a.sampleid = b.sampleid
	group by a.hpfid
	-----------------------------------------------------------
	-- get the total counts of all fields in @t1
	-----------------------------------------------------------
	declare @t3 table(
		hpfid int not null,
		cellcount float not null
		)
	--
	insert into @t3
	select a.hpfid, sum(a.CMembrane540) cellcount
	from @t1 b, wsi.dbo.CellTag a 
	where a.hpfid = b.hpfid
	and a.sampleid = b.sampleid
	and CMembrane540 is not null
	group by a.hpfid
	-----------------------------------------------------------
	-- get the densities of cd8 cells in the annotation region
	-----------------------------------------------------------
	declare @t4 table(
		sampleid int,
		hpfid int,
		area float,
		cd8count bigint,
		cd8_d float
		)
	insert into @t4
	select a.sampleid, a.hpfid, a.area,
		coalesce(b.cd8count,0) cd8count, coalesce(b.cd8count,0)/a.area cd8_d
	from @t1 a left outer join @t2 b on a.hpfid=b.hpfid
	-----------------------------------------------------------
	-- get the densities of all cells in the annotation region
	-----------------------------------------------------------
	declare @t5 table(
		sampleid int,
		hpfid int,
		area float,
		cellcount float,
		cell_d float
		)
	insert into @t5
	select a.sampleid, a.hpfid, a.area,
		coalesce(b.cellcount,0) cellcount, coalesce(b.cellcount,0)/a.area cell_d
	from @t1 a left outer join @t3 b on a.hpfid=b.hpfid
	-----------------------------------------------------------
	-- get the densities of all cells in the annotation region
	-----------------------------------------------------------
	insert into @out
	select a.sampleid, a.hpfid, a.area, cd8count, cd8_d, cellcount, cell_d,
		Row_Number() over(partition by a.sampleid order by cd8_d desc, cellcount desc) r
	from @t4 a, @t5 b 
		where a.sampleid = b.sampleid
		and a.hpfid = b.hpfid
	-- 
	return
END
GO
/*
IF OBJECT_ID(N'test.dbo.Hotspot', N'U') IS NOT NULL
    DROP TABLE test.dbo.Hotspot;
GO

select * 
into test.dbo.Hotspot
from dbo.fGetHotspotsC(1,1)

select sampleid, count(*) c 
from test.dbo.Hotspot
group by sampleid 
order by sampleid

*/