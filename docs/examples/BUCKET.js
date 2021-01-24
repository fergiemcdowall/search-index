(async () => {
  const cars = [
    {
      _id: 0,
      make: 'Volvo',
      colour: 'Black',
      year: 2011,
      price: 82580,
      model: 'XC90',
      drivetrain: 'Hybrid'
    },
    {
      _id: 1,
      make: 'Tesla',
      colour: 'White',
      year: 2012,
      price: 36693,
      model: 'S',
      drivetrain: 'Electric'
    },
    {
      _id: 2,
      make: 'Volvo',
      colour: 'Blue',
      year: 2005,
      price: 36130,
      model: 'XC60',
      drivetrain: 'Hybrid'
    },
    {
      _id: 3,
      make: 'Volvo',
      colour: 'Black',
      year: 2016,
      price: 42522,
      model: 'XC90',
      drivetrain: 'Diesel'
    },
    {
      _id: 4,
      make: 'Volvo',
      colour: 'Red',
      year: 2010,
      price: 60084,
      model: 'XC60',
      drivetrain: 'Petrol'
    },
    {
      _id: 5,
      make: 'BMW',
      colour: 'Black',
      year: 2013,
      price: 88503,
      model: '3-series',
      drivetrain: 'Diesel'
    },
    {
      _id: 6,
      make: 'Volvo',
      colour: 'Silver',
      year: 2011,
      price: 73297,
      model: 'XC60',
      drivetrain: 'Petrol'
    },
    {
      _id: 7,
      make: 'Volvo',
      colour: 'Silver',
      year: 2009,
      price: 55938,
      model: 'XC90',
      drivetrain: 'Petrol'
    },
    {
      _id: 8,
      make: 'Volvo',
      colour: 'Silver',
      year: 2010,
      price: 92375,
      model: 'XC60',
      drivetrain: 'Petrol'
    },
    {
      _id: 9,
      make: 'Tesla',
      colour: 'Red',
      year: 2008,
      price: 74017,
      model: 'X',
      drivetrain: 'Electric'
    }
  ]

  const si = require('../../')
  const db = await si({ name: 'BUCKET' })
  await db.PUT(cars)

  console.log('\nBUCKETING ->')
  await db.QUERY({ BUCKETS: 'make.Tesla' }).then(console.log)

  console.log('\nBUCKETING ->')
  await db.QUERY({ BUCKETS: 'make.BMW' }).then(console.log)

  console.log('\nBUCKETING ->')
  await db.QUERY({ BUCKETS: 'make.BMW' }).then(console.log)

  // TODO: this doesnt work properly- needs upstream fix in fii
  console.log('\nBUCKETING ->')
  await db.QUERY({
    BUCKET: { lte: 'year.2010' }
  }).then(console.log)

  console.log('\nBUCKETING ->')
  await db.QUERY({
    ALL: [
      { BUCKET: { gte: 'year.2000', lte: 'year.2008' } },
      { BUCKET: { gte: 'year.2009', lte: 'year.2020' } }
    ]
  }).then(console.log)
})()
