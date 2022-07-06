﻿// <auto-generated />
using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Migrations;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using Powermail.Data;

#nullable disable

namespace Powermail.Data.Migrations
{
    [DbContext(typeof(DataContext))]
    [Migration("20220507042755_Feed checkpoint")]
    partial class Feedcheckpoint
    {
        protected override void BuildTargetModel(ModelBuilder modelBuilder)
        {
#pragma warning disable 612, 618
            modelBuilder.HasAnnotation("ProductVersion", "6.0.4");

            modelBuilder.Entity("Powermail.Data.Models.Feed", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("INTEGER");

                    b.Property<int>("ErrorCount")
                        .HasColumnType("INTEGER");

                    b.Property<int?>("LastAccessCode")
                        .HasColumnType("INTEGER");

                    b.Property<string>("Name")
                        .HasColumnType("TEXT");

                    b.Property<DateTime?>("Timestamp")
                        .HasColumnType("TEXT");

                    b.Property<string>("Url")
                        .IsRequired()
                        .HasColumnType("TEXT");

                    b.HasKey("Id");

                    b.ToTable("Feeds");
                });

            modelBuilder.Entity("Powermail.Data.Models.FeedItem", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("INTEGER");

                    b.Property<int>("FeedId")
                        .HasColumnType("INTEGER");

                    b.Property<string>("InternalId")
                        .IsRequired()
                        .HasColumnType("TEXT");

                    b.Property<DateTime>("Timestamp")
                        .HasColumnType("TEXT");

                    b.Property<string>("Title")
                        .IsRequired()
                        .HasColumnType("TEXT");

                    b.Property<string>("Url")
                        .IsRequired()
                        .HasColumnType("TEXT");

                    b.HasKey("Id");

                    b.HasIndex("FeedId");

                    b.ToTable("FeedItems");
                });

            modelBuilder.Entity("Powermail.Data.Models.User", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("INTEGER");

                    b.Property<string>("Email")
                        .IsRequired()
                        .HasColumnType("TEXT");

                    b.Property<TimeSpan>("FeedInterval")
                        .HasColumnType("TEXT");

                    b.Property<DateTime?>("FeedTimestamp")
                        .HasColumnType("TEXT");

                    b.Property<bool>("IsAdmin")
                        .HasColumnType("INTEGER");

                    b.Property<string>("Name")
                        .HasColumnType("TEXT");

                    b.Property<string>("TimeZone")
                        .IsRequired()
                        .HasColumnType("TEXT");

                    b.HasKey("Id");

                    b.ToTable("Users");
                });

            modelBuilder.Entity("Powermail.Data.Models.UserFeed", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("INTEGER");

                    b.Property<DateTime?>("Checkpoint")
                        .HasColumnType("TEXT");

                    b.Property<int>("FeedId")
                        .HasColumnType("INTEGER");

                    b.Property<bool>("IsPrivate")
                        .HasColumnType("INTEGER");

                    b.Property<string>("Name")
                        .HasColumnType("TEXT");

                    b.Property<int>("UserId")
                        .HasColumnType("INTEGER");

                    b.HasKey("Id");

                    b.HasIndex("FeedId");

                    b.HasIndex("UserId");

                    b.ToTable("UserFeeds");
                });

            modelBuilder.Entity("Powermail.Data.Models.FeedItem", b =>
                {
                    b.HasOne("Powermail.Data.Models.Feed", null)
                        .WithMany("Items")
                        .HasForeignKey("FeedId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();
                });

            modelBuilder.Entity("Powermail.Data.Models.UserFeed", b =>
                {
                    b.HasOne("Powermail.Data.Models.Feed", "Feed")
                        .WithMany()
                        .HasForeignKey("FeedId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.HasOne("Powermail.Data.Models.User", "User")
                        .WithMany("Feeds")
                        .HasForeignKey("UserId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.Navigation("Feed");

                    b.Navigation("User");
                });

            modelBuilder.Entity("Powermail.Data.Models.Feed", b =>
                {
                    b.Navigation("Items");
                });

            modelBuilder.Entity("Powermail.Data.Models.User", b =>
                {
                    b.Navigation("Feeds");
                });
#pragma warning restore 612, 618
        }
    }
}
